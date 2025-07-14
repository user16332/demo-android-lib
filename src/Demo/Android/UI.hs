{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tokens.Android.UI
  ( Event(Event)
  , EventDetails(..)
  , Node(Node)
  , View(Parent, Leaf)
  , Ctrl(CtrlText)
  , TextView(TextView)
  , JContext
  , JActivity
  , ActivityEventType(..)
  , ObjectId(ObjectId)
  , registerPorts
  , notifyUIUpdate
  , initUI
  ) where

import           Control.Concurrent                  (runInBoundThread)
import           Control.Concurrent.MVar             (MVar, tryTakeMVar, putMVar, takeMVar, newMVar)
import           Control.DeepSeq                     (NFData)
import           Control.Exception                   (SomeException)
import           Control.Exception.Safe              (isSyncException)
import           Control.Lens.TH                     (makeLenses)
import           Control.Monad                       (when, (<=<))
import           Control.Monad.Catch                 (handleJust, handle)
import           Control.Monad.Except                (MonadError, liftEither)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Data.Bool                           (bool)
import           Data.Default                        (Default(def))
import           Data.Foldable                       (for_, find)
import           Data.Int                            (Int32)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Foreign.JNI                         (JNINativeMethod(JNINativeMethod), JVMException,
                                                      getObjectClass, showException, isSameObject,
                                                      registerNatives, newGlobalRef, runInAttachedThread)
import           Foreign.JNI.Types                   (JClass, JType(Class, Void), JNIEnv(..),
                                                      JObject, jnull, objectFromPtr)
import           Foreign.Ptr                         (Ptr, FunPtr)
import           GHC.Generics                        (Generic)
import           Language.Java                       (J(J), JString,
                                                      Interpretation(Interp), Reflect, Reify(reify),
                                                      SJType(SClass),
                                                      call, reflect, reify, new, getClass,
                                                      unsafeCast, getStaticField,
                                                      methodSignature, callStatic)
import           Prelude.Singletons                  (SingI(sing), Sing, SomeSing(SomeSing))

import           Tokens.Android.Log                  (debug, info, err)
import           Tokens.Android.System               (JContext, IntentFilter, JXId, JIntent)
import           Tokens.XId                          (XId)

handleException_ :: IO a -> IO a
handleException_ =
  handleJust (find isSyncException . Just) (\(ex :: SomeException) -> (err . T.pack . show $ ex) >>= pure undefined)
    . handle (\(ex :: JVMException) -> (showException ex >>= err) >>= pure undefined)

handleException :: (MonadIO m, MonadError Text m) => IO a -> m a
handleException =
  liftEither <=< liftIO . handleJust (find isSyncException . Just) (\(ex :: SomeException) -> pure . Left . T.pack . show $ ex)
    . handle (\(ex :: JVMException) -> fmap Left $ showException ex) . fmap Right

type JActivity = J ('Class "android.app.Activity")
type JView = J ('Class "android.view.View")

--instance Interpretation URI where
--  type Interp URI = 'Class "android.net.Uri"
--
--instance Reify URI where
--  reify = mkURI <=< reify

data TextView
  = TextView
  deriving (Eq, Ord, Show, Generic, NFData)
makeLenses ''TextView

data Ctrl
  = CtrlText
    { _txTextView :: TextView
    , _txText :: Text
    } deriving (Eq, Ord, Show, Generic, NFData)
makeLenses ''Ctrl

data ViewGroup
  = FrameLayout
  deriving (Eq, Ord, Show, Generic, NFData)

data View
  = Parent
    { _paViewGroup :: ViewGroup
    }
  | Leaf
    { _lfCtrl :: Ctrl
    }
  deriving (Eq, Ord, Show, Generic, NFData)
makeLenses ''View

data Node = Node
  { _ndLayoutParams :: ()
  , _ndView :: View
  }
  deriving (Eq, Show, Generic, NFData)
makeLenses ''Node

data JNodeCache = JNodeCache JView [JNodeCache]

javaClass :: Node -> Text
javaClass (Node _ (Leaf (CtrlText TextView _))) = "android.widget.TextView"
javaClass _ = undefined

createNode :: JActivity -> Node -> IO (Node, JNodeCache)
createNode jactivity (Node _ (Leaf (CtrlText TextView _))) = do
  -- TODO: check initial text
  jtextview <- new (unsafeCast jactivity :: JContext) >>= newGlobalRef :: IO (J ('Class "android.widget.TextView"))
  pure (Node def (Leaf (CtrlText TextView "")), JNodeCache (unsafeCast jtextview) [])
createNode _ _ = undefined

syncText :: J ('Class "android.widget.TextView") -> Text -> Text -> IO Text
syncText jtextview text text' = do
  when (text /= text') $ do
    jstring <- reflect text'
    call jtextview "setText" (unsafeCast jstring :: J ('Class "java.lang.CharSequence"))
  pure text'

syncCtrl :: JActivity -> JView -> Ctrl -> Ctrl -> IO Ctrl
syncCtrl jactivity jview (CtrlText textView text) (CtrlText textView' text') = do
  text'' <- syncText (unsafeCast jview) text text'
  textView'' <- case (textView, textView') of
    (TextView, TextView) -> pure TextView
    _ -> undefined -- TODO
  pure $ CtrlText textView'' text''
syncCtrl _ _ _ _ = undefined

syncView :: JActivity -> JView -> View -> [JNodeCache] -> View -> IO (View, [JNodeCache])
syncView jactivity jview (Parent viewGroup) caches (Parent viewGroup') = undefined
syncView jactivity jview (Leaf ctrl) [] (Leaf ctrl') = do
  ctrl'' <- syncCtrl jactivity jview ctrl ctrl'
  pure (Leaf ctrl'', [])
syncView _ _ _ _ _ = undefined -- TODO

syncNode :: JActivity -> JView -> Node -> [JNodeCache] -> Node -> IO (Node, [JNodeCache])
syncNode jactivity jview (Node params view) caches (Node params' view') = do
  -- TODO: compare first
--  params' <- syncLayoutParams jnode view view`
  let params'' = params
  (view'', caches'') <- syncView jactivity jview view caches view'
  pure (Node params'' view'', caches'')
syncNode _ _ _ _ _ = undefined

syncActivity :: JActivity -> (Node, JNodeCache) -> Node -> IO (Node, JNodeCache)
syncActivity jactivity (rendered, caches) node = do
  (rendered', JNodeCache jview' caches') <- flip (bool $ pure (rendered, caches)) (javaClass rendered /= javaClass node) $ do
    (rendered', cachedView'@(JNodeCache jview' _)) <- createNode jactivity node
    call jactivity "setContentView" jview' :: IO ()
    pure (rendered', cachedView')
  (rendered'', caches'') <- syncNode jactivity jview' rendered' caches'  node
  pure (rendered'', JNodeCache jview' caches'')

newtype ObjectId = ObjectId Int32
  deriving (Eq, Ord, Show)
  deriving newtype NFData

objectId :: JObject -> IO ObjectId
objectId jobj = fmap ObjectId (callStatic "java.lang.System" "identityHashCode" jobj :: IO Int32)

data ActivityEventType
  = ActivityCreate
  | ActivityRestart
  | ActivityStart
  | ActivityResume
  | ActivityPause
  | ActivityStop
  | ActivityDestroy
  deriving (Eq, Ord, Show, Generic, NFData, Bounded, Enum)

data EventDetails
  = ActivityEvent JActivity ActivityEventType
  | BluetoothGattEvent
  deriving (Eq, Show, Generic, NFData)

instance Interpretation EventDetails where
  type Interp EventDetails = 'Class "p2p.tokens.Events.Event"

instance Reify EventDetails where
  reify jevent = do
    jclass <- getObjectClass jevent
    jstring <- call jclass "getName" :: IO JString
    className <- reify jstring :: IO Text
    case className of
      "p2p.tokens.MainActivity$Event" -> do
        let jactivityEvent = unsafeCast jevent :: J ('Class "p2p.tokens.MainActivity$Event")
        jactivity <- call jactivityEvent "getActivity" >>= newGlobalRef :: IO JActivity -- TODO: newGLobalRef?
        code <- call jactivityEvent "getCode" :: IO Int32
        pure . ActivityEvent jactivity . toEnum . fromIntegral $ code
      "p2p.tokens.BluetoothGattCallback$Event" -> pure BluetoothGattEvent
      _ -> undefined -- TODO

data Event = Event XId EventDetails
  deriving (Eq, Show, Generic, NFData)

type UIUpdateCallback = JNIEnv -> Ptr JClass -> Ptr JActivity -> IO ()
foreign import ccall "wrapper" wrapUIUpdate :: UIUpdateCallback -> IO (FunPtr UIUpdateCallback)

type UIEventCallback = JNIEnv -> Ptr JClass -> Ptr JXId -> Ptr (J ('Class "p2p.tokens.Events.Event")) -> IO ()
foreign import ccall "wrapper" wrapUIEvent :: UIEventCallback -> IO (FunPtr UIEventCallback)

registerPorts :: MVar Node -> MVar Event -> IO ()
registerPorts uiUpdatePort uiEventPort = do
  renderedPort <- newMVar (jnull, (Node def (Leaf (CtrlText TextView "")), JNodeCache jnull []))
  uiUpdatePtr <- wrapUIUpdate $ \_ _ jactivityPtr -> handleException_ $ do
    opt <- tryTakeMVar uiUpdatePort
    for_ opt $ \node -> do
      (jactivity, rendered) <- takeMVar renderedPort
      jactivity' <- objectFromPtr jactivityPtr
      isSameActivity <- isSameObject jactivity jactivity'
      (jactivity'', rendered'') <- flip (bool $ pure (jactivity, rendered)) (not isSameActivity) $ do
        rendered'' <- initUI jactivity'
        jactivity'' <- newGlobalRef jactivity'
        pure (jactivity'', rendered'')
      rendered''' <- syncActivity jactivity'' rendered'' node
      putMVar renderedPort (jactivity'', rendered''')
  uiEventPtr <- wrapUIEvent $ \_ _ jeventIdPtr jeventPtr -> handleException_ $ do
    eventId <- objectFromPtr jeventIdPtr >>= reify
    details <- objectFromPtr jeventPtr >>= reify
    putMVar uiEventPort $ Event eventId details
  clazz <- getClass (SClass "p2p.tokens.Events") >>= newGlobalRef
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
        "onUIEvent"
        (methodSignature
          [ SomeSing (sing :: Sing ('Class "p2p.tokens.XId"))
          , SomeSing (sing :: Sing ('Class "p2p.tokens.Events$Event"))
          ]
          (sing :: Sing 'Void)
        )
        uiEventPtr
    ]

notifyUIUpdate :: JActivity -> IO ()
notifyUIUpdate jactivity = do
  runInBoundThread . runInAttachedThread . handleException_ $
    call (unsafeCast jactivity :: J ('Class "java.lang.Runnable")) "run" :: IO ()

initUI :: JActivity -> IO (Node, JNodeCache)
initUI jactivity = handleException_ $ do
  jlabel <- new (unsafeCast jactivity :: JContext) >>= newGlobalRef :: IO (J ('Class "android.widget.TextView"))
  jwrap <- getStaticField "android.view.ViewGroup$LayoutParams" "WRAP_CONTENT" :: IO Int32
  jgravity <- getStaticField "android.view.Gravity" "CENTER" :: IO Int32
  jlayout <- new jwrap jwrap jgravity :: IO (J ('Class "android.widget.FrameLayout$LayoutParams"))
  call
    (unsafeCast jactivity :: J ('Class "androidx.appcompat.app.AppCompatActivity"))
    "setContentView"
    (unsafeCast jlabel :: J ('Class "android.view.View"))
    (unsafeCast jlayout :: J ('Class "android.view.ViewGroup$LayoutParams")) :: IO ()
  pure (Node def (Leaf (CtrlText TextView "")), JNodeCache (unsafeCast jlabel) [])
