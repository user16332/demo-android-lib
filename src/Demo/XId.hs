{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE TemplateHaskell            #-}

module Tokens.XId
  ( XId
  , fromByteString
  , toByteString
  , randomXId
  , xid
  ) where

import           Codec.Serialise.Class               (Serialise(encode, decode))
import           Control.DeepSeq                     (NFData)
import           Control.Error.Util                  (hush)
import           Control.Monad                       (guard)
import           Crypto.Hash                         (Digest, MD5, hash)
import           Data.Binary                         (Binary(put, get))
import           Data.ByteArray                      (convert)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as BS (replicate)
import qualified Data.ByteString.Base16              as B16 (encode, decode)
import qualified Data.ByteString.UTF8                as BS (toString, fromString)
import           Data.Char                           (isSpace)
import           Data.Default                        (Default(def))
import           Data.Either                         (fromRight)
import           Data.Foldable                       (find)
import           Data.Maybe                          (fromMaybe)
import           GHC.Generics                        (Generic)
import           Refined                             (Refined, SizeEqualTo, unrefine, refine)
import           System.Entropy                      (getEntropy)

newtype XId = XId (Refined (SizeEqualTo 16) ByteString) deriving (Eq, Ord, Generic, NFData)

fromByteString :: ByteString -> Maybe XId
fromByteString = fmap XId . hush . refine

toByteString :: XId -> ByteString
toByteString (XId refined) = unrefine refined

instance Show XId where
  show (XId d) = BS.toString . B16.encode . unrefine $ d

instance Read XId where
  readsPrec _ s = fromMaybe [] $ do
    (hex, rest) <- fmap (splitAt 32) . find ((>= 32) . length) . Just . dropWhile isSpace $ s
    guard $ null rest
    bs <- hush . B16.decode . BS.fromString $ hex
    refined <- hush . refine $ bs
    pure [(XId refined, rest)]

instance Serialise XId where
  decode = maybe (fail "invalid xid") pure . fromByteString =<< decode
  encode = encode . toByteString

randomXId :: IO XId
randomXId =
  fromMaybe undefined . fromByteString <$> getEntropy 16

instance Default XId where
  def = fromMaybe undefined . fromByteString $ BS.replicate 16 0

instance Binary XId where
  put (XId bs) = put . unrefine $ bs
  get = get >>= either (\ex -> fail $ "invalid binary XId: " <> show ex) (pure . XId) . refine

xid :: ByteString -> XId
xid = XId . fromRight undefined . refine . convert . (hash :: ByteString -> Digest MD5)
