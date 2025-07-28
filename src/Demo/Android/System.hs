{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Demo.Android.System
  ( Event(Event)
  , EventDetails(..)
  , JContext
  , JActivity
  , ActivityEventType(..)
  , registerPorts
  , notifyUIUpdate
  , beep
  ) where

import           Control.Concurrent                  (runInBoundThread, forkIO)
import           Control.Concurrent.MVar             (MVar, tryTakeMVar, putMVar, takeMVar, newMVar)
import           Control.Exception                   (SomeException)
import           Control.Exception.Safe              (isSyncException)
import           Control.Monad.Catch                 (handleJust, handle)
import           Data.Bool                           (bool)
import           Data.Foldable                       (for_, find)
import           Data.Functor                        (void)
import           Data.Int                            (Int32)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Foreign.JNI                         (JNINativeMethod(JNINativeMethod), JVMException,
                                                      getObjectClass, showException, isSameObject,
                                                      registerNatives, newGlobalRef, runInAttachedThread)
import           Foreign.JNI.Types                   (JClass, JType(Class, Void, Prim), JNIEnv(..),
                                                      JObjectArray, jnull, objectFromPtr)
import           Foreign.Ptr                         (Ptr, FunPtr)
import           GHC.Generics                        (Generic)
import           Language.Java                       (J(J), JString, Interpretation(Interp), Reify(reify),
                                                      SJType(SClass), call, reify, new, getClass, unsafeCast,
                                                      getStaticField, methodSignature)
import           Prelude.Singletons                  (SingI(sing), Sing, SomeSing(SomeSing))

import           Demo.Android.Log                    (debug, info, err)

handleException_ :: IO a -> IO a
handleException_ =
  handleJust (find isSyncException . Just) (\(ex :: SomeException) -> (err . T.pack . show $ ex) >>= pure undefined)
    . handle (\(ex :: JVMException) -> (showException ex >>= err) >>= pure undefined)

type JContext = J ('Class "android.content.Context")
type JActivity = J ('Class "android.app.Activity")

data ActivityEventType
  = ActivityCreate
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

data EventDetails
  = ActivityEvent JActivity ActivityEventType
  | MethodInvocation Text JObjectArray
  deriving (Eq, Show, Generic)

instance Interpretation EventDetails where
  type Interp EventDetails = 'Class "com.example.haskell_demo.Events.Event"

instance Reify EventDetails where
  reify event = do
    clazz <- getObjectClass event
    className <- call clazz "getName" >>= (reify :: JString -> IO Text)
    case className of
      "com.example.haskell_demo.MainActivity$Event" -> do
        let activityEvent = unsafeCast event :: J ('Class "com.example.haskell_demo.MainActivity$Event")
        activity <- call activityEvent "getActivity" >>= newGlobalRef :: IO JActivity -- TODO: newGLobalRef?
        code <- call activityEvent "getCode" :: IO Int32
        pure . ActivityEvent activity . toEnum . fromIntegral $ code
      "com.example.haskell_demo.VoidInvocationHandler$Event" -> do
        let methodInvocation = unsafeCast event :: J ('Class "com.example.haskell_demo.VoidInvocationHandler$Event")
        method <- call methodInvocation "getMethod" >>= newGlobalRef >>= (reify :: JString -> IO Text) -- TODO: newGLobalRef?
        args <- call methodInvocation "getArgs" :: IO JObjectArray
        pure $ MethodInvocation method args
      _ -> undefined -- TODO

data Event = Event Int32 EventDetails
  deriving (Eq, Show, Generic)

type UIUpdateCallback = JNIEnv -> Ptr JClass -> Ptr JActivity -> IO ()
foreign import ccall "wrapper" wrapUIUpdate :: UIUpdateCallback -> IO (FunPtr UIUpdateCallback)

type UIEventCallback = JNIEnv -> Ptr JClass -> Int32 -> Ptr (J ('Class "com.example.haskell_demo.Events.Event")) -> IO ()
foreign import ccall "wrapper" wrapUIEvent :: UIEventCallback -> IO (FunPtr UIEventCallback)

registerPorts :: MVar a -> MVar Event -> (JActivity -> IO()) -> (JActivity -> a -> IO()) ->IO ()
registerPorts uiUpdatePort uiEventPort initUI updateUI = do
  activityCache <- newMVar (jnull :: JActivity)
  uiUpdatePtr <- wrapUIUpdate $ \_ _ activityPtr -> handleException_ $ do
    opt <- tryTakeMVar uiUpdatePort
    for_ opt $ \ui -> do
      activity <- takeMVar activityCache
      activity' <- objectFromPtr activityPtr
      isSameActivity <- isSameObject activity activity'
      activity'' <- flip (bool $ pure activity) (not isSameActivity) $ do
        initUI activity'
        activity'' <- newGlobalRef activity'
        pure activity''
      updateUI activity'' ui
      putMVar activityCache activity''
  uiEventPtr <- wrapUIEvent $ \_ _ eventId eventPtr -> handleException_ $ do
    details <- objectFromPtr eventPtr >>= reify
    putMVar uiEventPort $ Event eventId details
  clazz <- getClass (SClass "com.example.haskell_demo.Events") >>= newGlobalRef
  registerNatives clazz
    [ JNINativeMethod
        "onUIUpdate"
        (methodSignature
          [ SomeSing (sing :: Sing ('Class "android.app.Activity"))
          ]
          (sing :: Sing 'Void)
        )
        uiUpdatePtr
    , JNINativeMethod
        "onEvent"
        (methodSignature
          [ SomeSing (sing :: Sing ('Prim "int"))
          , SomeSing (sing :: Sing ('Class "com.example.haskell_demo.Events$Event"))
          ]
          (sing :: Sing 'Void)
        )
        uiEventPtr
    ]

notifyUIUpdate :: JActivity -> IO ()
notifyUIUpdate activity =
  runInBoundThread . runInAttachedThread . handleException_ $
--    call (unsafeCast activity :: J ('Class "com.example.haskell_demo.MainActivity")) "postUIUpdate" :: IO ()
    -- TODO: hack
    call (unsafeCast activity :: J ('Class "java.lang.Runnable")) "run" :: IO ()

beep :: IO ()
beep =
  void . forkIO . runInBoundThread . runInAttachedThread . handleException_ $ do
    musicStream <- getStaticField "android.media.AudioManager" "STREAM_MUSIC" :: IO Int32
    toneGenerator <- new musicStream (100 :: Int32) :: IO (J ('Class "android.media.ToneGenerator"))
    beepTone <- getStaticField "android.media.ToneGenerator" "TONE_PROP_ACK" :: IO Int32
    void $ (call toneGenerator "startTone" beepTone (150 :: Int32) :: IO Bool)
