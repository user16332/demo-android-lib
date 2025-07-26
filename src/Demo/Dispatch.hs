{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Demo.Dispatch () where

import           Control.Concurrent.Async            (async)
import           Control.Concurrent.Loops            (Step(Step), drive, loop, runStep, pair)
import           Control.DeepSeq                     (force)
import           Control.Monad                       ((<=<))
import           Control.Monad.Except                (MonadError, runExceptT)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Control.Monad.Reader                (MonadReader, runReaderT)
import           Data.Functor                        (void)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Foreign.JNI                         (newGlobalRef, jniInit)
import           Foreign.JNI.Types                   (objectFromPtr)
import           Foreign.Ptr                         (Ptr)
import           Language.Java                       (J(J), JNIEnv(..))

import           Demo.Android.Log                    (debug, info, err, runLoggingT)
import           Demo.Android.System                 (JContext)
import           Demo.UI                             (ui)

type DispatchContext r m =
  ( MonadIO m
  , MonadReader r m
  , MonadError Text m
  )

dispatch :: (DispatchContext r m) => JContext -> m (Step () ())
dispatch jctx = do
  step <- ui jctx
  let go state = Step $ \() -> do
        let step0 = force state
        ((), step1) <- runStep step0 ()
        pure ((), go step1)
  pure . loop () . go $ step

foreign export ccall "demo_start" start :: Ptr JNIEnv -> Ptr JContext -> IO ()

start :: Ptr JNIEnv -> Ptr JContext -> IO ()
start jni jctxPtr = either err pure <=< runExceptT $ do
  liftIO $ jniInit jni
  jctx <- liftIO $ objectFromPtr jctxPtr >>= newGlobalRef
  step <- flip runReaderT () $ dispatch jctx
  liftIO . void . async . drive $ step
