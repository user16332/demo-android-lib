{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Demo.Main () where

import           Control.Applicative                 ((<|>))
import           Control.Concurrent                  (threadDelay, forkIO)
import           Control.Concurrent.MVar             (MVar, tryTakeMVar, putMVar, takeMVar, newEmptyMVar)
import           Control.Concurrent.Async            (async, race)
import           Control.Exception                   (SomeException)
import           Control.Exception.Safe              (isSyncException)
import           Control.Monad                       ((<=<), when, forever)
import           Control.Monad.Catch                 (handleJust, handle, uninterruptibleMask_)
import           Control.Monad.Except                (MonadError, runExceptT, liftEither)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Control.Monad.Loops                 (iterateM_)
import           Control.Monad.Reader                (MonadReader, runReaderT)
import           Data.Foldable                       (for_, find)
import           Data.Functor                        (void)
import           Data.Int                            (Int32)
import           Data.Maybe                          (fromMaybe, catMaybes, isNothing)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Data.Time.Clock                     (getCurrentTime)
import           Data.Time.Format                    (formatTime, defaultTimeLocale)
import           Foreign.JNI                         (JVMException, showException, newGlobalRef, jniInit,
                                                      runInAttachedThread, isSameObject)
import           Foreign.JNI.Types                   (objectFromPtr)
import           Foreign.Ptr                         (Ptr)
import           Language.Java                       (J(J), JNIEnv(..))

import           Demo.Android.Log                    (debug, info, err, runLoggingT)
import           Demo.Android.UI                     (JContext, EventDetails(ActivityEvent, MethodInvocation),
                                                      JActivity, ActivityEventType(ActivityCreate), Event(Event),
                                                      beep, registerPorts, notifyUIUpdate, buttonClickEventId)

mainActivityEventId :: Int32
mainActivityEventId = 1000

loop :: JContext -> MVar Text -> MVar Event -> IO ()
loop jctx uiUpdatePort uiEventPort = do
  flip iterateM_ (Nothing, "") $ \(activityOpt0, ui0) -> do
    uiEventOpt <- (either Just $ const Nothing) <$> race (takeMVar uiEventPort) (threadDelay 100000 {- 100 ms-})
    let activityOpt1 =
          (uiEventOpt >>= \case Event eventId (ActivityEvent activity ActivityCreate) | eventId == mainActivityEventId -> Just activity; _ -> Nothing) <|> activityOpt0
    for_ uiEventOpt $ \case Event eventId (MethodInvocation _ _) | eventId == buttonClickEventId -> beep; _ -> pure ()
    utc <- getCurrentTime
    let ui1 = T.pack $ formatTime defaultTimeLocale "%H:%M:%S" utc
    for_ activityOpt1 $ \activity ->
      when (ui1 /= ui0 || isNothing activityOpt0) $ do
        void $ tryTakeMVar uiUpdatePort
        putMVar uiUpdatePort ui1
        notifyUIUpdate activity
    pure (activityOpt1, ui1)

foreign export ccall "demo_start" start :: Ptr JNIEnv -> Ptr JContext -> IO ()

start :: Ptr JNIEnv -> Ptr JContext -> IO ()
start jni jctxPtr = do
  jniInit jni
  jctx <- objectFromPtr jctxPtr >>= newGlobalRef
  uiUpdatePort <- newEmptyMVar
  uiEventPort <- newEmptyMVar
  registerPorts uiUpdatePort uiEventPort
  void . forkIO $ loop jctx uiUpdatePort uiEventPort
