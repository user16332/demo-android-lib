{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE TemplateHaskell     #-}

module Demo.Events
  ( Context
  , Message(..)
  , Event(..)
  , Ref(..)
  , Ext(Ext)
  , EventPattern(..)
  , coi
  , apply
  , mapExt
  , refData
  , refCtr
  , refRng
  , forRef
  , forMaybe
  , onEmpty
  , maybeDecode
  , forLens
  , forLensOpt
  , forUnit
  , forEither
  ) where

import           Control.Arrow                       (second)
import           Control.DeepSeq                     (NFData)
import           Control.Lens                        (makeLenses, Lens', Traversal', _Nothing,
                                                      (^.), (%~), _Just, _Left, _Right, (^?))
import           Control.Error.Util                  (hush)
import           Control.Monad                       ((<=<))
import           Data.Binary                         (Binary(put, get), decodeOrFail)
import           Data.Binary.Get                     (getWord32le, getRemainingLazyByteString)
import           Data.Binary.Put                     (putWord32le, putLazyByteString)
import           Data.Bool                           (bool)
import qualified Data.ByteString.Lazy                as BL (ByteString, null, unpack)
import           Data.Dynamic                        (Dynamic, fromDyn)
import           Data.Foldable                       (find)
import           Data.Monoid                         (Endo(Endo, appEndo))
import           Data.Set                            (Set)
import qualified Data.Set                            as Set (fromList)
import           Data.Word                           (Word32, Word64)
import           GHC.Generics                        (Generic)
import           System.Posix.Types                  (Fd)
import           System.Random.SplitMix              (SMGen)

import           Demo.XId                            (XId)

type Counter = Word64 -- TODO

type Context = Word32

data Message = Message
  { msgCtx :: Context
  , msgPayload :: BL.ByteString
  } deriving (Eq, Ord, Generic)
makeLenses ''Message

instance Show Message where
  show (Message ctx payload) = show (ctx, BL.unpack payload)

instance NFData Message

instance Binary Message where
  put (Message ctx payload) = do
    putWord32le ctx
    putLazyByteString payload
  get = Message <$> getWord32le <*> getRemainingLazyByteString

maybeDecode :: Binary a => Dynamic -> Maybe a
maybeDecode =
  (\(rest, _, x) -> find (const . BL.null $ rest) . Just $ x) <=< hush . decodeOrFail . flip fromDyn undefined

data Event = Event EventPattern Dynamic
  deriving Show

data EventPattern -- TODO: "range". "dimension", "axis", "subspace"?
  = EventTimestamp
  | EventUI XId
  | EventOff
  deriving (Eq, Show)

newtype Ext a = Ext [(EventPattern, Dynamic -> Endo a)] -- TODO: map?

instance Semigroup (Ext a) where
  (Ext parts1) <> (Ext parts2) = Ext $ parts1 <> parts2

instance Monoid (Ext a) where
  mempty = Ext mempty

coi :: Ext a -> [EventPattern]
coi (Ext parts) = fmap fst parts

-- TODO: ByteString? Ord?
apply :: Ext a -> [Event] -> Endo a
apply (Ext parts) events =
  -- TODO: reverse endos?
  flip foldMap events $ \(Event pattern dyn) -> flip foldMap parts $
    foldMap (($ dyn) . snd) . find ((== pattern) . fst) . Just

mapExt :: (Endo a -> Endo b) -> Ext a -> Ext b
mapExt f (Ext parts) =
  Ext $ fmap (second $ fmap f) parts

data Ref a = Ref
  { _refRng :: SMGen
  , _refCtr :: Counter
  , _refData :: a
  } deriving (Show, Generic, NFData)
makeLenses ''Ref

forRef :: (Ref a -> Ext (Ref a)) -> (a -> Ext a) -> (Ref a) -> Ext (Ref a)
forRef pr pa item =
  pr item <> (forLens refData pa) item

onEmpty :: Endo a -> Dynamic -> Endo a
onEmpty fn =
  bool (Endo id) fn . BL.null . flip fromDyn undefined

forLens :: Lens' a b -> (b -> Ext b) -> a -> Ext a
forLens lens f =
  mapExt (Endo . (lens %~) . appEndo) . f . (^. lens)

forLensOpt :: Traversal' a b -> (b -> Ext b) -> a -> Ext a
forLensOpt lens f =
  mapExt (Endo . (lens %~) . appEndo) . foldMap f . (^? lens)

forMaybe :: Ext (Maybe a) -> (a -> Ext (Maybe a)) -> (a -> Ext a) -> Maybe a -> Ext (Maybe a)
forMaybe pn pj pa item =
  let n = foldMap (\() -> pn) (item ^? _Nothing)
      j = foldMap pj (item ^? _Just) <> (forLensOpt _Just pa) item
  in n <> j

forUnit :: () -> Ext ()
forUnit () = mempty

forEither :: (a -> Ext (Either a b)) -> (b -> Ext (Either a b)) -> (a -> Ext a) -> (b -> Ext b) -> Either a b -> Ext (Either a b)
forEither lr rl pa pb item =
  let l = foldMap lr (item ^? _Left) <> (forLensOpt _Left pa) item
      r = foldMap rl (item ^? _Right) <> (forLensOpt _Right pb) item
  in l <> r
