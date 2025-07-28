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
import           Data.Traversable                    (for)
import           Data.Time.Clock                     (getCurrentTime)
import           Data.Time.Format                    (formatTime, defaultTimeLocale)
import           Foreign.JNI                         (JVMException, showException, newGlobalRef, jniInit,
                                                      runInAttachedThread, isSameObject)
import           Foreign.JNI.Types                   (objectFromPtr)
import           Foreign.Ptr                         (Ptr)
import           Language.Java                       (J(J), JNIEnv(..), unsafeCast)

import           Demo.Android.Log                    (debug, info, err, runLoggingT)
import           Demo.Android.System                 (JContext, EventDetails(ActivityEvent, MethodInvocation),
                                                      JActivity, ActivityEventType(ActivityCreate), Event(Event),
                                                      registerPorts, notifyUIUpdate, beep)
import           Demo.Android.UI                     (JView, JTextView, activityFindViewById,
                                                      textViewSetText, activitySetContentView,
                                                      frameLayoutAddView, mkFrameLayout, mkVerticalLayout,
                                                      mkButton, mkTextView, linearLayoutAddView)

mainActivityEventId :: Int32
mainActivityEventId = 1000

buttonClickEventId :: Int32
buttonClickEventId = 1001

textViewId :: Int32
textViewId = 123

initUI :: JActivity -> IO ()
initUI activity = do
  let ctx = unsafeCast activity :: JContext
  linearLayout <- mkVerticalLayout ctx
  do
    frameLayout <- mkFrameLayout ctx
    linearLayoutAddView linearLayout (unsafeCast frameLayout :: JView)
    label <- mkTextView ctx 50.0 (Just textViewId)
    frameLayoutAddView frameLayout (unsafeCast label :: JView)
  do
    frameLayout <- mkFrameLayout ctx
    linearLayoutAddView linearLayout (unsafeCast frameLayout :: JView)
    button <- mkButton ctx "Beep!" buttonClickEventId
    frameLayoutAddView frameLayout (unsafeCast button :: JView)
  activitySetContentView activity (unsafeCast linearLayout :: JView)

updateUI :: JActivity -> Text -> IO ()
updateUI activity text = do
  label <- activityFindViewById activity textViewId
  textViewSetText (unsafeCast label :: JTextView) text

loop :: MVar Text -> MVar Event -> IO ()
loop uiUpdatePort uiEventPort = do
  flip iterateM_ (Nothing, "") $ \(mainActivityOpt0, ui0) -> do
    uiEventOpt <- (either Just $ const Nothing) <$> race (takeMVar uiEventPort) (threadDelay 100000 {- 100 ms-})
    mainActivityOpt1 <- flip (maybe $ pure mainActivityOpt0) uiEventOpt $ \case
      Event eventId (ActivityEvent activity ActivityCreate) | eventId == mainActivityEventId -> pure $ Just activity
      Event eventId (MethodInvocation _ _) | eventId == buttonClickEventId -> beep *> pure mainActivityOpt0
    utc <- getCurrentTime
    let ui1 = T.pack $ formatTime defaultTimeLocale "%H:%M:%S" utc
    for_ mainActivityOpt1 $ \activity ->
      when (ui1 /= ui0 || isNothing mainActivityOpt0) $ do
        void $ tryTakeMVar uiUpdatePort
        putMVar uiUpdatePort ui1
        notifyUIUpdate activity
    pure (mainActivityOpt1, ui1)

foreign export ccall "demo_start" start :: Ptr JNIEnv -> Ptr JContext -> IO ()

start :: Ptr JNIEnv -> Ptr JContext -> IO ()
start jni _ = do
  jniInit jni
  uiUpdatePort <- newEmptyMVar
  uiEventPort <- newEmptyMVar
  registerPorts uiUpdatePort uiEventPort initUI updateUI
  void . forkIO $ loop uiUpdatePort uiEventPort
