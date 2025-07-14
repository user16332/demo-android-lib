{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokens.Dispatch () where

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

import           Tokens.Android.Keystore             (getPublicKey, sign)
import           Tokens.Android.Log                  (debug, info, err, runLoggingT)
import           Tokens.Android.System               (JContext)
import           Tokens.Auth                         (auth)
import           Tokens.UI                           (ui)
import           Tokens.USB                          (usb)

type DispatchContext r m =
  ( MonadIO m
  , MonadReader r m
  , MonadError Text m
  )

dispatch :: (DispatchContext r m) => JContext -> m (Step () ())
dispatch jctx = do
  let keyAlias = "id"
  idPublicKey <- getPublicKey keyAlias
  liftIO . debug . T.pack . show $ idPublicKey
  usbStep <- usb jctx
  authStep <- runLoggingT $ auth idPublicKey usbStep
  step <- liftA2 pair (ui jctx) $ pure authStep
  let go state = Step $ \() -> do
        let step0 = force state
        (((), _), step1) <- runStep step0 ((), (idPublicKey, mempty))
        pure ((), go step1)
  pure . loop () . go $ step

foreign export ccall "tokens_start" start :: Ptr JNIEnv -> Ptr JContext -> IO ()

start :: Ptr JNIEnv -> Ptr JContext -> IO ()
start jni jctxPtr = either err pure <=< runExceptT $ do
  liftIO $ jniInit jni
  jctx <- liftIO $ objectFromPtr jctxPtr >>= newGlobalRef
  step <- flip runReaderT () $ dispatch jctx
  liftIO . void . async . drive $ step
