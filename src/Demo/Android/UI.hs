{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Demo.Android.UI
  ( JView
  , JTextView
  , mkTextView
  , mkButton
  , mkFrameLayout
  , mkListener
  , mkVerticalLayout
  , activitySetContentView
  , activityFindViewById
  , frameLayoutAddView
  , linearLayoutAddView
  , textViewSetText
  ) where

import           Data.Foldable                       (for_)
import           Data.Int                            (Int32)
import           Data.Text                           (Text)
import           Foreign.JNI.Types                   (JType(Class), JObject)
import           Language.Java                       (J(J), JString, SJType(SClass), toArray,
                                                      call, reflect, new, getClass, unsafeCast, getStaticField,
                                                      callStatic)

import           Demo.Android.System                 (JContext, JActivity)

type JView = J ('Class "android.view.View")
type JViewGroup = J ('Class "android.view.ViewGroup")
type JLinearLayout = J ('Class "android.widget.LinearLayout")
type JFrameLayout = J ('Class "android.widget.FrameLayout")
type JTextView = J ('Class "android.widget.TextView")

activitySetContentView :: JActivity -> JView -> IO ()
activitySetContentView activity view =
  call activity "setContentView" view

activityFindViewById :: JActivity -> Int32 -> IO JView
activityFindViewById activity viewId =
  call activity "findViewById" viewId

mkVerticalLayout :: JContext -> IO JLinearLayout
mkVerticalLayout ctx = do
  layout <- new ctx
  vertical <- getStaticField "android.widget.LinearLayout" "VERTICAL" :: IO Int32
  call layout "setOrientation" vertical :: IO ()
  pure layout

mkTextView :: JContext -> Float -> Maybe Int32 -> IO JTextView
mkTextView ctx size viewIdOpt = do
  textView <- new ctx
  for_ viewIdOpt $ \viewId ->
    call (unsafeCast textView :: JView) "setId" viewId :: IO ()
  call textView "setTextSize" size :: IO ()
  pure textView

mkListener :: String -> Int32 -> IO JObject
mkListener interfaceName eventId = do
  handler <- new eventId :: IO (J ('Class "com.example.haskell_demo.VoidInvocationHandler"))
  interface <- getClass (SClass interfaceName)
  classLoader <- call interface "getClassLoader" :: IO (J ('Class "java.lang.ClassLoader"))
  interfaces <- toArray [ interface ]
  callStatic
    "java.lang.reflect.Proxy"
    "newProxyInstance"
    classLoader
    interfaces
    (unsafeCast handler :: J ('Class "java.lang.reflect.InvocationHandler"))

mkButton :: JContext -> Text -> Int32 -> IO (J ('Class "android.widget.Button"))
mkButton ctx label eventId = do
  button <- new ctx
  text <- reflect label
  call
    (unsafeCast button :: JTextView)
    "setText"
    (unsafeCast text :: J ('Class "java.lang.CharSequence")) :: IO ()
  listener <- mkListener "android.view.View$OnClickListener" eventId
  call
    (unsafeCast button :: JView)
    "setOnClickListener"
    (unsafeCast listener :: J ('Class "android.view.View$OnClickListener")) :: IO ()
  pure button

mkFrameLayout :: JContext -> IO JFrameLayout
mkFrameLayout ctx = new ctx

linearLayoutAddView :: JLinearLayout -> JView -> IO ()
linearLayoutAddView linearLayout view = do
  wrapContent <- getStaticField "android.view.ViewGroup$LayoutParams" "WRAP_CONTENT" :: IO Int32
  matchParent <- getStaticField "android.view.ViewGroup$LayoutParams" "MATCH_PARENT" :: IO Int32
  layoutParams <- new matchParent wrapContent (1.0 :: Float) :: IO (J ('Class "android.widget.LinearLayout$LayoutParams"))
  call
    (unsafeCast linearLayout :: JViewGroup)
    "addView"
    view
    (unsafeCast layoutParams :: J ('Class "android.view.ViewGroup$LayoutParams"))

frameLayoutAddView :: JFrameLayout -> JView -> IO ()
frameLayoutAddView frameLayout view = do
  wrapContent <- getStaticField "android.view.ViewGroup$LayoutParams" "WRAP_CONTENT" :: IO Int32
  gravityCenter <- getStaticField "android.view.Gravity" "CENTER" :: IO Int32
  layoutParams <- new wrapContent wrapContent gravityCenter :: IO (J ('Class "android.widget.FrameLayout$LayoutParams"))
  call
    (unsafeCast frameLayout :: JViewGroup)
    "addView"
    view
    (unsafeCast layoutParams :: J ('Class "android.view.ViewGroup$LayoutParams"))

textViewSetText :: JTextView -> Text -> IO ()
textViewSetText textView text =
  reflect text >>= call textView "setText" . (unsafeCast :: JString -> J ('Class "java.lang.CharSequence"))
