{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tokens.Android.Keystore
  ( getPublicKey
  , sign
  ) where

import           Control.Arrow                       (left)
import           Control.Exception                   (SomeException)
import           Control.Exception.Safe              (isSyncException)
import           Control.Monad                       ((<=<))
import           Control.Monad.Catch                 (handleJust, handle)
import           Control.Monad.Except                (MonadError, liftEither)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Crypto.Hash                         (Digest)
import           Crypto.Hash.Algorithms              (SHA256)
import           Data.ASN1.Encoding                  (decodeASN1)
import           Data.ASN1.BinaryEncoding            (DER(DER))
import           Data.ASN1.Parse                     (runParseASN1, getNextContainer)
import           Data.ASN1.Types                     (ASN1ConstructionType(Sequence), ASN1(IntVal))
import           Data.ByteArray                      (convert)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy                as BL (fromStrict)
import           Data.Bits                           (shift, (.|.))
import           Data.Bool                           (bool)
import qualified Data.ByteString                     as BS (foldl')
import qualified Data.ByteString.Builder             as BL (word8, toLazyByteString)
import qualified Data.ByteString.Lazy                as BL (toStrict)
import           Data.Foldable                       (find)
import           Data.Functor                        (void)
import           Data.Int                            (Int32)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Foreign.JNI                         (JVMException, showException)
import           Foreign.JNI.Types                   (JType(Class, Iface), JCharArray, jnull)
import           IDIoT.Types                         (PublicKey(PublicKey), Signature(Signature),
                                                      toBytes32, toBytes32)
import           Language.Java                       (J(J), JString, JByteArray,
                                                      Interpretation(Interp), Reflect, Reify(reify),
                                                      call, reflect, reify, new, toArray,
                                                      unsafeCast, getStaticField, callStatic)

import           Tokens.Android.Log                  (debug, info, err)

type AndroidContext m =
  ( MonadIO m
  , MonadError Text m
  )

handleException :: (MonadIO m, MonadError Text m) => IO a -> m a
handleException =
  liftEither <=< liftIO . handleJust (find isSyncException . Just) (\(ex :: SomeException) -> pure . Left . T.pack . show $ ex)
    . handle (\(ex :: JVMException) -> fmap Left $ showException ex) . fmap Right

type JCertificate = J ('Iface "java.security.cert.Certificate")
type JPublicKey = J ('Iface "java.security.PublicKey")
type JKeySpecBuilder = J ('Class "android.security.keystore.KeyGenParameterSpec$Builder")

instance Interpretation Integer where
  type Interp Integer = 'Class "java.math.BigInteger"

instance Reify Integer where
  reify jbigint = do
    jbytes <- call jbigint "toByteArray" :: IO JByteArray
    bytes <- reify jbytes
    pure . BS.foldl' (\a b -> a `shift` 8 .|. fromIntegral b) 0 $ bytes

instance Reflect Integer where
  reflect int = do
    let go n a = flip (bool mempty) (n > 0) . (go (pred n :: Int) (a `shift` (-8)) <>) . BL.word8 . fromIntegral $ a
    let bytes = BL.toStrict . BL.toLazyByteString . go 32 $ int
    jbytes <- reflect bytes :: IO JByteArray
    new jbytes

getPublicKey :: AndroidContext m => Text -> m PublicKey
getPublicKey alias = handleException $ do
  jprovider <- reflect ("AndroidKeyStore" :: Text) :: IO JString
  jkeystore <- callStatic "java.security.KeyStore" "getInstance" jprovider :: IO (J ('Class "java.security.KeyStore"))
  call jkeystore "load" (jnull :: J ('Class "java.security.KeyStore$LoadStoreParameter")) :: IO ()
  jpubkey <- do
    jalias <- reflect alias :: IO JString
    jprivkey <- call jkeystore "getKey" jalias (jnull :: JCharArray) :: IO (J ('Class "java.security.Key"))
    flip (bool $ ((call jkeystore "getCertificate" jalias :: IO JCertificate) >>= \jcert -> call jcert "getPublicKey" :: IO JPublicKey)) (jprivkey == jnull) $ do
      jalgo <- getStaticField "android.security.keystore.KeyProperties" "KEY_ALGORITHM_EC" :: IO JString
      jgenerator <- callStatic "java.security.KeyPairGenerator" "getInstance" jalgo jprovider :: IO (J ('Class "java.security.KeyPairGenerator"))
      jspec <- do
        purpose <- getStaticField "android.security.keystore.KeyProperties" "PURPOSE_SIGN" :: IO Int32
        jdigest <- getStaticField "android.security.keystore.KeyProperties" "DIGEST_NONE" :: IO JString
        jdigests <- toArray [jdigest]
        jbuilder <- new jalias purpose :: IO JKeySpecBuilder
        jcurve <- reflect ("secp256r1" :: Text) :: IO JString
        jalgoSpec <- new jcurve :: IO (J ('Class "java.security.spec.ECGenParameterSpec"))
        void $ (call jbuilder "setAlgorithmParameterSpec" (unsafeCast jalgoSpec :: J ('Class "java.security.spec.AlgorithmParameterSpec")) :: IO JKeySpecBuilder)
        void $ (call jbuilder "setDigests" jdigests :: IO JKeySpecBuilder)
        call jbuilder "build" :: IO (J ('Class "android.security.keystore.KeyGenParameterSpec"))
      call jgenerator "initialize" (unsafeCast jspec :: J ('Class "java.security.spec.AlgorithmParameterSpec")) :: IO ()
      jkeypair <- call jgenerator "generateKeyPair" :: IO (J ('Class "java.security.KeyPair"))
      call jkeypair "getPublic" :: IO JPublicKey
  jpoint <- call (unsafeCast jpubkey :: J ('Iface "java.security.interfaces.ECPublicKey")) "getW" :: IO (J ('Class "java.security.spec.ECPoint"))
  x <- call jpoint "getAffineX" >>= reify :: IO Integer
  y <- call jpoint "getAffineY" >>= reify :: IO Integer
  pure $ PublicKey (toBytes32 x) (toBytes32 y)

sign :: AndroidContext m => Text -> Digest SHA256 -> m Signature
sign alias digest = do
  bytes <- handleException $ do
    jprovider <- reflect ("AndroidKeyStore" :: Text) :: IO JString
    jkeystore <- callStatic "java.security.KeyStore" "getInstance" jprovider :: IO (J ('Class "java.security.KeyStore"))
    call jkeystore "load" (jnull :: J ('Class "java.security.KeyStore$LoadStoreParameter")) :: IO ()
    jprivkey <- do
      jalias <- reflect alias :: IO JString
      call jkeystore "getKey" jalias (jnull :: JCharArray) :: IO (J ('Class "java.security.Key"))
    jsignature <- do
      jalgo <- reflect ("NONEwithECDSA" :: Text) :: IO JString
      callStatic "java.security.Signature" "getInstance" jalgo :: IO (J ('Class "java.security.Signature"))
    call jsignature "initSign" (unsafeCast jprivkey :: J ('Class "java.security.PrivateKey")) :: IO ()
    jdigest <- reflect . (convert :: Digest SHA256 -> ByteString) $ digest :: IO JByteArray
    call jsignature "update" jdigest :: IO ()
    jbytes <- call jsignature "sign" :: IO JByteArray
    reify jbytes
  asn1 <- liftEither . left (T.pack . show) . decodeASN1 DER . BL.fromStrict $ bytes
  liftEither . left T.pack . flip runParseASN1 asn1 $ do
    list <- getNextContainer Sequence
    case list of
      [IntVal r, IntVal s] -> pure $ Signature (toBytes32 r) (toBytes32 s)
      _ -> fail "invalid signature format"
