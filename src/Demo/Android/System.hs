{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tokens.Android.System
  ( JContext
  , JIntent
  , JXId
  , JIntentFilter
  , ObjectId(ObjectId)
  , Intent
  , IntentFilter(IntentFilter)
  , beep
  ) where

import           Control.DeepSeq                     (NFData(rnf))
import           Control.Exception                   (SomeException)
import           Control.Exception.Safe              (isSyncException)
import           Control.Lens.TH                     (makeLenses)
import           Control.Monad                       ((<=<))
import           Control.Monad.Catch                 (handleJust, handle)
import           Control.Monad.Except                (MonadError, liftEither)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Data.Foldable                       (for_, find)
import           Data.Functor                        (void)
import           Data.Int                            (Int32)
import           Data.Maybe                          (fromMaybe)
import           Data.Set                            (Set)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Foreign.JNI                         (JVMException, showException)
import           Foreign.JNI.Types                   (JType(Class), JObject)
import           GHC.Generics                        (Generic)
import           Language.Java                       (J(J), JString, JByteArray,
                                                      Interpretation(Interp), Reflect, Reify(reify),
                                                      call, reflect, reify, new,
                                                      unsafeCast, getStaticField, callStatic)

import           Tokens.Android.Log                  (debug, info, err)
import           Tokens.XId                          (XId, fromByteString, toByteString)

handleException_ :: IO a -> IO a
handleException_ =
  handleJust (find isSyncException . Just) (\(ex :: SomeException) -> (err . T.pack . show $ ex) >>= pure undefined)
    . handle (\(ex :: JVMException) -> (showException ex >>= err) >>= pure undefined)

handleException :: (MonadIO m, MonadError Text m) => IO a -> m a
handleException =
  liftEither <=< liftIO . handleJust (find isSyncException . Just) (\(ex :: SomeException) -> pure . Left . T.pack . show $ ex)
    . handle (\(ex :: JVMException) -> fmap Left $ showException ex) . fmap Right

type JContext = J ('Class "android.content.Context")
type JIntent = J ('Class "android.content.Intent")
type JIntentFilter = J ('Class "android.content.IntentFilter")
type JXId = J ('Class "p2p.tokens.XId")

instance NFData (J a) where
  rnf jctx = jctx `seq` () -- TODO: ???

instance Interpretation XId where
  type Interp XId = 'Class "p2p.tokens.XId"

instance Reify XId where
  reify jxid = do
    jbytes <- call jxid "getBytes" :: IO JByteArray
    bytes <- reify jbytes
    -- TODO: exception
    pure $ fromMaybe undefined $ fromByteString bytes

instance Reflect XId where
  reflect xid = do
    jbytes <- reflect . toByteString $ xid
    callStatic "p2p.tokens.XId" "create" jbytes

data Intent a = Intent
  { _inAction :: Text
  , _inExtras :: a
  }
  deriving (Eq, Ord, Show, Generic, NFData)
makeLenses ''Intent

instance Interpretation a => Interpretation (Intent a) where
  type Interp (Intent a) = 'Class "android.content.Intent"

instance Reify a => Reify (Intent a) where
  reify jintent = do
    jstring <- call jintent "getAction" :: IO JString
    action <- reify jstring
    jbundle <- call jintent "getExtras" :: IO (J ('Class "android.os.Bundle"))
    extras <- reify (unsafeCast jbundle :: J (Interp a)) -- TODO: ouch
    pure $ Intent action extras

data IntentFilter = IntentFilter
  { _ifActions :: Set Text
  }
  deriving (Eq, Ord, Show, Generic, NFData)
makeLenses ''IntentFilter

instance Interpretation IntentFilter where
  type Interp IntentFilter = 'Class "android.content.IntentFilter"

instance Reflect IntentFilter where
  reflect (IntentFilter actions) = do
    jfilter <- new :: IO JIntentFilter
    for_ actions $ \action -> do
      jstring <- reflect action :: IO JString
      call jfilter "addAction" jstring :: IO ()
    pure jfilter

newtype ObjectId = ObjectId Int32
  deriving (Eq, Ord, Show, Generic, NFData)

objectId :: JObject -> IO ObjectId
objectId jobj = fmap ObjectId (callStatic "java.lang.System" "identityHashCode" jobj :: IO Int32)

--data FileId = FileId Posix.DeviceID Posix.FileID
--  deriving (Eq, Ord, Show, Generic, NFData)

beep :: IO ()
beep = handleException_ $ do
  musicStream <- getStaticField "android.media.AudioManager" "STREAM_MUSIC" :: IO Int32
  toneGenerator <- new musicStream (100 :: Int32) :: IO (J ('Class "android.media.ToneGenerator"))
  beepTone <- getStaticField "android.media.ToneGenerator" "TONE_PROP_BEEP" :: IO Int32
  void $ (call toneGenerator "startTone" beepTone (150 :: Int32) :: IO Bool)
