{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tokens.Android.USB
  ( UsbAccessory(UsbAccessory)
  , Event(Event)
  , EventDetails(USBEvent)
  , subscribeUsbAccessory
  , discoverUsbAccessory
  , registerPort
  ) where

import           Control.Concurrent                  (runInBoundThread)
import           Control.Concurrent.MVar             (MVar, putMVar)
import           Control.DeepSeq                     (NFData(rnf))
import           Control.Exception                   (SomeException)
import           Control.Exception.Safe              (isSyncException)
import           Control.Lens                        ((^?), _head)
import           Control.Lens.TH                     (makeLenses)
import           Control.Monad                       ((<=<))
import           Control.Monad.Catch                 (handleJust, handle)
import           Control.Monad.Except                (MonadError, liftEither)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Data.Foldable                       (find)
import           Data.Functor                        (void)
import           Data.Int                            (Int32)
import qualified Data.Set                            as Set (fromList)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Data.Traversable                    (for)
import           Foreign.JNI                         (JNINativeMethod(JNINativeMethod), JVMException,
                                                      getObjectClass, showException, registerNatives,
                                                      newGlobalRef, runInAttachedThread)
import qualified Foreign.JNI.String                  as JNI (String)
import           Foreign.JNI.Types                   (JNIEnv(..), JClass, JType(Class, Void, Prim),
                                                      JObjectArray, JObject, jnull, objectFromPtr)
import           Foreign.Ptr                         (Ptr, FunPtr)
import           GHC.Generics                        (Generic)
import           Language.Java                       (J(J), JString, JArray,
                                                      Interpretation(Interp), Reify(reify),
                                                      SJType(SClass),
                                                      call, reflect, reify, new, getClass,
                                                      unsafeCast, type (<>), getStaticField,
                                                      methodSignature)
import           Prelude.Singletons                  (SingI(sing), Sing, SomeSing(SomeSing))
import           System.IO                           (BufferMode(NoBuffering), Handle, hSetBuffering)
import           System.Posix.IO                     (fdToHandle)
import           System.Posix.Types                  (Fd(Fd))

import           Tokens.Android.Log                  (debug, info, err)
import           Tokens.Android.System               (JContext, JIntent, IntentFilter(IntentFilter), Intent)

handleException_ :: IO a -> IO a
handleException_ =
  handleJust (find isSyncException . Just) (\(ex :: SomeException) -> (err . T.pack . show $ ex) >>= pure undefined)
    . handle (\(ex :: JVMException) -> (showException ex >>= err) >>= pure undefined)

handleException :: (MonadIO m, MonadError Text m) => IO a -> m a
handleException =
  liftEither <=< liftIO . handleJust (find isSyncException . Just) (\(ex :: SomeException) -> pure . Left . T.pack . show $ ex)
    . handle (\(ex :: JVMException) -> fmap Left $ showException ex) . fmap Right

type JUsbManager = J ('Class "android.hardware.usb.UsbManager")
type JUsbAccessory = J ('Class "android.hardware.usb.UsbAccessory")

instance NFData Fd where
  rnf (Fd fd) = fd `seq` ()

instance NFData Handle where
  rnf h = h `seq` ()

data UsbAccessoryBundle = UsbAccessoryBundle JUsbAccessory
  deriving (Eq, Show, Generic, NFData)
makeLenses ''UsbAccessoryBundle

instance Interpretation UsbAccessoryBundle where
  type Interp UsbAccessoryBundle = 'Class "android.os.Bundle"

instance Reify UsbAccessoryBundle where
  reify jbundle = do
    jstring <- getStaticField "android.hardware.usb.UsbManager" "EXTRA_ACCESSORY" :: IO JString
    -- TODO: global?
    jaccessory <- call jbundle "getParcelable" jstring >>= newGlobalRef :: IO JUsbAccessory
    pure $ UsbAccessoryBundle jaccessory

newtype UsbAccessory = UsbAccessory Handle
  deriving (Eq, Show)
  deriving newtype NFData

discoverUsbAccessory :: (MonadIO m, MonadError Text m) => JContext -> Maybe UsbAccessory -> m (Maybe UsbAccessory)
discoverUsbAccessory jctx accOpt = handleException . runInBoundThread . runInAttachedThread $ do
  jusbManager <- do
    jusbService <- getStaticField "android.content.Context" "USB_SERVICE" :: IO JString
    unsafeCast <$> (call jctx "getSystemService" jusbService :: IO JObject) :: IO JUsbManager
  jaccessories <- do
    jarray <- call jusbManager "getAccessoryList" :: IO (JArray ('Class "android.hardware.usb.UsbAccessory"))
    maybe (pure []) reify . find (/= jnull) . Just $ jarray :: IO [JUsbAccessory]
--  debug . T.pack . show $ jaccessories
--  debug . T.pack . show =<< traverse objectId (fmap upcast jaccessories)
  for (jaccessories ^? _head) $ \jaccesory ->
    flip (flip maybe $ pure) accOpt $ do
      jpfd <- call jusbManager "openAccessory" jaccesory :: IO (J ('Class "android.os.ParcelFileDescriptor"))
      fd <- call jpfd "detachFd" :: IO Int32
      fh <- fdToHandle . fromIntegral $ fd
      hSetBuffering fh NoBuffering
      pure . UsbAccessory $ fh

--  flip (maybe $ pure Nothing) (jaccessories ^? _head) $ \jaccesory -> do
--    jpfd <- call jusbManager "openAccessory" jaccesory :: IO (J ('Class "android.os.ParcelFileDescriptor"))
--    flip (bool $ pure accOpt) (jpfd /= jnull) $ do
--      fd <- call jpfd "detachFd" :: IO Int32
--      h <- fdToHandle . fromIntegral $ fd
--      pure . Just . UsbAccessory $ h

--  (oldAccOpt, newAccOpt) <- flip (maybe $ pure (accOpt, Nothing)) (jaccessories ^? _head) $ \jaccesory -> do
--    objId' <- fmap ObjId (callStatic "java.lang.System" "identityHashCode" $ upcast jaccesory :: IO Int32)
--    flip (flip maybe $ pure . (Nothing,) . Just) (find (\(UsbAccessory objId _) -> objId == objId') accOpt) $ do
--      jpfd <- call jusbManager "openAccessory" jaccesory :: IO (J ('Class "android.os.ParcelFileDescriptor"))
--      fd' <- call jpfd "detachFd" :: IO Int32
--      pure . (accOpt,) . Just . UsbAccessory objId' . fromIntegral $ fd'
--  for_ oldAccOpt (\(UsbAccessory _ fd) -> closeFd fd)
--  pure newAccOpt

registerBroadcastReceiver :: JContext -> IntentFilter -> IO (J ('Class "android.content.BroadcastReceiver"))
registerBroadcastReceiver jctx ifilter = do
  jreceiver <- new :: IO (J ('Class "p2p.tokens.USBBroadcastReceiver"))
  jfilter <- reflect ifilter
--  jflag <- getStaticField "androidx.core.content.ContextCompat" "RECEIVER_EXPORTED" :: IO Int32
  void $ (call jctx "registerReceiver" (unsafeCast jreceiver :: J ('Class "android.content.BroadcastReceiver")) jfilter :: IO JIntent) -- TODO: ensure null return?
  pure . unsafeCast $ jreceiver -- TODO: newGlobalRef?

--unregisterBroadcastReceiver :: JContext -> J ('Class "android.content.BroadcastReceiver") -> IO ()
--unregisterBroadcastReceiver jctx jreceiver = do
--  call jctx "unregisterReceiver" jreceiver
  --  TODO: deleteGlobalRef jreceiver?

subscribeUsbAccessory :: (MonadIO m, MonadError Text m) => JContext -> m (J ('Class "android.content.BroadcastReceiver"))
subscribeUsbAccessory jctx = handleException $ do
  actions <- fmap Set.fromList $ traverse (reify <=< (getStaticField "android.hardware.usb.UsbManager" :: JNI.String -> IO JString))
    ["ACTION_USB_ACCESSORY_ATTACHED", "ACTION_USB_ACCESSORY_DETACHED"]
  registerBroadcastReceiver jctx $ IntentFilter actions

type EventCallback = JNIEnv -> Ptr JClass -> Ptr (J ('Class "p2p.tokens.Events.Event")) -> IO ()
foreign import ccall "wrapper" wrapEventCallback :: EventCallback -> IO (FunPtr EventCallback)

data EventDetails
  = USBEvent (Intent ())
  deriving (Eq, Show, Generic)
  deriving anyclass NFData

instance Interpretation EventDetails where
  type Interp EventDetails = 'Class "p2p.tokens.Events.Event"

instance Reify EventDetails where
  reify jevent = do
    jclass <- getObjectClass jevent
    jstring <- call jclass "getName" :: IO JString
    className <- reify jstring :: IO Text
    case className of
      "p2p.tokens.USBBroadcastReceiver$Event" -> do
        jintent <- call (unsafeCast jevent :: J ('Class "p2p.tokens.USBBroadcastReceiver$Event")) "getIntent" :: IO JIntent
--        jaction <- call jintent "getAction" :: IO JString
--        action <- reify jaction :: IO Text
--        -- TODO: meh
--        intent <- case action of
--          "android.hardware.usb.action.USB_ACCESSORY_ATTACHED" -> reify jintent :: IO (Intent UsbAccessoryBundle)
--          "android.hardware.usb.action.USB_ACCESSORY_DETACHED" -> reify jintent :: IO (Intent UsbAccessoryBundle)
--          _ -> undefined
        intent <- reify jintent
        pure $ USBEvent intent
      _ -> undefined -- TODO

data Event = Event EventDetails
  deriving (Eq, Show, Generic, NFData)

registerPort :: MVar Event -> IO ()
registerPort eventPort = do
  eventPtr <- wrapEventCallback $ \_ _ jeventPtr -> handleException_ $ do
    details <- objectFromPtr jeventPtr >>= reify
    putMVar eventPort $ Event details
  clazz <- getClass (SClass "p2p.tokens.Events") >>= newGlobalRef
  registerNatives clazz
    [ JNINativeMethod
        "onUSBEvent"
        (methodSignature
          [ SomeSing (sing :: Sing ('Class "p2p.tokens.Events$Event"))
          ]
          (sing :: Sing 'Void)
        )
        eventPtr
    ]
