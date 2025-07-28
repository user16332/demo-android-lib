{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Demo.Android.UI
  ( Event(Event)
  , EventDetails(..)
  , JContext
  , JActivity
  , ActivityEventType(..)
  , registerPorts
  , notifyUIUpdate
  , initUI
  , buttonClickEventId
  , beep
  ) where

import           Control.Concurrent                  (runInBoundThread, forkIO)
import           Control.Concurrent.MVar             (MVar, tryTakeMVar, putMVar, takeMVar, newMVar)
import           Control.Exception                   (SomeException)
import           Control.Exception.Safe              (isSyncException)
import           Control.Monad                       (when, (<=<))
import           Control.Monad.Catch                 (handleJust, handle)
import           Control.Monad.Except                (MonadError, liftEither)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
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
                                                      JObject, JObjectArray, jnull, objectFromPtr)
import           Foreign.Ptr                         (Ptr, FunPtr)
import           GHC.Generics                        (Generic)
import           Language.Java                       (J(J), JString,
                                                      Interpretation(Interp), Reflect, Reify(reify),
                                                      SJType(SClass), toArray,
                                                      call, reflect, reify, new, getClass,
                                                      unsafeCast, getStaticField,
                                                      methodSignature, callStatic)
import           Prelude.Singletons                  (SingI(sing), Sing, SomeSing(SomeSing))

import           Demo.Android.Log                    (debug, info, err)

textViewId :: Int32
textViewId = 123

buttonClickEventId :: Int32
buttonClickEventId = 1001

handleException_ :: IO a -> IO a
handleException_ =
  handleJust (find isSyncException . Just) (\(ex :: SomeException) -> (err . T.pack . show $ ex) >>= pure undefined)
    . handle (\(ex :: JVMException) -> (showException ex >>= err) >>= pure undefined)

type JContext = J ('Class "android.content.Context")
type JActivity = J ('Class "android.app.Activity")
type JView = J ('Class "android.view.View")

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
  reify jevent = do
    jclass <- getObjectClass jevent
    jstring <- call jclass "getName" :: IO JString
    className <- reify jstring :: IO Text
    case className of
      "com.example.haskell_demo.MainActivity$Event" -> do
        let activityEvent = unsafeCast jevent :: J ('Class "com.example.haskell_demo.MainActivity$Event")
        activity <- call activityEvent "getActivity" >>= newGlobalRef :: IO JActivity -- TODO: newGLobalRef?
        code <- call activityEvent "getCode" :: IO Int32
        pure . ActivityEvent activity . toEnum . fromIntegral $ code
      "com.example.haskell_demo.VoidInvocationHandler$Event" -> do
        let methodInvocation = unsafeCast jevent :: J ('Class "com.example.haskell_demo.VoidInvocationHandler$Event")
        jstring <- call methodInvocation "getMethod" >>= newGlobalRef :: IO JString -- TODO: newGLobalRef?
        method <- reify jstring
        args <- call methodInvocation "getArgs" :: IO JObjectArray
        pure $ MethodInvocation method args
      _ -> undefined -- TODO

data Event = Event Int32 EventDetails
  deriving (Eq, Show, Generic)

type UIUpdateCallback = JNIEnv -> Ptr JClass -> Ptr JActivity -> IO ()
foreign import ccall "wrapper" wrapUIUpdate :: UIUpdateCallback -> IO (FunPtr UIUpdateCallback)

type UIEventCallback = JNIEnv -> Ptr JClass -> Int32 -> Ptr (J ('Class "com.example.haskell_demo.Events.Event")) -> IO ()
foreign import ccall "wrapper" wrapUIEvent :: UIEventCallback -> IO (FunPtr UIEventCallback)

registerPorts :: MVar Text -> MVar Event -> IO ()
registerPorts uiUpdatePort uiEventPort = do
  activityCache <- newMVar (jnull :: JActivity)
  uiUpdatePtr <- wrapUIUpdate $ \_ _ activityPtr -> handleException_ $ do
    opt <- tryTakeMVar uiUpdatePort
    for_ opt $ \text -> do
      activity <- takeMVar activityCache
      activity' <- objectFromPtr activityPtr
      isSameActivity <- isSameObject activity activity'
      activity'' <- flip (bool $ pure activity) (not isSameActivity) $ do
        initUI activity'
        activity'' <- newGlobalRef activity'
        pure activity''
      label <- call
        (unsafeCast activity'' :: J ('Class "androidx.appcompat.app.AppCompatActivity"))
        "findViewById"
        textViewId :: IO JView
      string <- reflect text
      call
        (unsafeCast label :: J ('Class "android.widget.TextView"))
        "setText"
        (unsafeCast string :: J ('Class "java.lang.CharSequence")) :: IO ()
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
notifyUIUpdate activity = do
  runInBoundThread . runInAttachedThread . handleException_ $
    call (unsafeCast activity :: J ('Class "java.lang.Runnable")) "run" :: IO ()

initUI :: JActivity -> IO ()
initUI activity = handleException_ $ do
  wrapContent <- getStaticField "android.view.ViewGroup$LayoutParams" "WRAP_CONTENT" :: IO Int32
  matchParent <- getStaticField "android.view.ViewGroup$LayoutParams" "MATCH_PARENT" :: IO Int32
  gravityCenter <- getStaticField "android.view.Gravity" "CENTER" :: IO Int32

  linearLayout <- new (unsafeCast activity :: JContext) :: IO (J ('Class "android.widget.LinearLayout"))

  do
    vertical <- getStaticField "android.widget.LinearLayout" "VERTICAL" :: IO Int32
    call linearLayout "setOrientation" vertical :: IO ()
    call
      (unsafeCast activity :: J ('Class "androidx.appcompat.app.AppCompatActivity"))
      "setContentView"
      (unsafeCast linearLayout :: JView) :: IO ()

  do
    frameLayout <- new (unsafeCast activity :: JContext) :: IO (J ('Class "android.widget.FrameLayout"))
    do
      layoutParams <- new matchParent wrapContent (1.0 :: Float) :: IO (J ('Class "android.widget.LinearLayout$LayoutParams"))
      call
        (unsafeCast linearLayout :: J ('Class "android.view.ViewGroup"))
        "addView"
        (unsafeCast frameLayout :: JView)
        (unsafeCast layoutParams :: J ('Class "android.view.ViewGroup$LayoutParams")) :: IO ()

    do
      label <- new (unsafeCast activity :: JContext) :: IO (J ('Class "android.widget.TextView"))
      call
        (unsafeCast label :: JView)
        "setId"
        textViewId :: IO ()
      call
        (unsafeCast label :: J ('Class "android.widget.TextView"))
        "setTextSize"
        (50.0 :: Float) :: IO ()
      layoutParams <- new wrapContent wrapContent gravityCenter :: IO (J ('Class "android.widget.FrameLayout$LayoutParams"))
      call
        (unsafeCast frameLayout :: J ('Class "android.view.ViewGroup"))
        "addView"
        (unsafeCast label :: JView)
        (unsafeCast layoutParams :: J ('Class "android.view.ViewGroup$LayoutParams")) :: IO ()

  do
    frameLayout <- new (unsafeCast activity :: JContext) :: IO (J ('Class "android.widget.FrameLayout"))
    do
      layoutParams <- new matchParent wrapContent (1.0 :: Float) :: IO (J ('Class "android.widget.LinearLayout$LayoutParams"))
      call
        (unsafeCast linearLayout :: J ('Class "android.view.ViewGroup"))
        "addView"
        (unsafeCast frameLayout :: JView)
        (unsafeCast layoutParams :: J ('Class "android.view.ViewGroup$LayoutParams")) :: IO ()

    do
      button <- new (unsafeCast activity :: JContext) :: IO (J ('Class "android.widget.Button"))
      text <- reflect ("Beep!" :: Text)
      call
        (unsafeCast button :: J ('Class "android.widget.TextView"))
        "setText"
        (unsafeCast text :: J ('Class "java.lang.CharSequence")) :: IO ()
      do
        handler <- new buttonClickEventId :: IO (J ('Class "com.example.haskell_demo.VoidInvocationHandler"))
        interface <- getClass (SClass "android.view.View$OnClickListener")
        classLoader <- call interface "getClassLoader" :: IO (J ('Class "java.lang.ClassLoader"))
        interfaces <- toArray [ interface ]
        proxy <- callStatic
          "java.lang.reflect.Proxy"
          "newProxyInstance"
          classLoader
          interfaces
          (unsafeCast handler :: J ('Class "java.lang.reflect.InvocationHandler")) :: IO JObject
        call
          (unsafeCast button :: J ('Class "android.view.View"))
          "setOnClickListener"
          (unsafeCast proxy :: J ('Class "android.view.View$OnClickListener")) :: IO ()

      layoutParams <- new wrapContent wrapContent gravityCenter :: IO (J ('Class "android.widget.FrameLayout$LayoutParams"))
      call
        (unsafeCast frameLayout :: J ('Class "android.view.ViewGroup"))
        "addView"
        (unsafeCast button :: JView)
        (unsafeCast layoutParams :: J ('Class "android.view.ViewGroup$LayoutParams")) :: IO ()

beep :: IO ()
beep =
  void . forkIO . runInBoundThread . runInAttachedThread . handleException_ $ do
    musicStream <- getStaticField "android.media.AudioManager" "STREAM_MUSIC" :: IO Int32
    toneGenerator <- new musicStream (100 :: Int32) :: IO (J ('Class "android.media.ToneGenerator"))
    beepTone <- getStaticField "android.media.ToneGenerator" "TONE_PROP_ACK" :: IO Int32
    void $ (call toneGenerator "startTone" beepTone (150 :: Int32) :: IO Bool)
