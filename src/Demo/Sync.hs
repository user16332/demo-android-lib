{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokens.Sync
  ( sync
  ) where

import           Control.Concurrent.Loops            (Step(Step), loop, pair, runStep)
import           Control.DeepSeq                     (NFData, force)
import           Control.Error.Util                  (hush)
import           Control.Lens                        (view)
import           Control.Monad                       (guard, unless, (<=<))
import           Control.Monad.Catch                 (uninterruptibleMask_)
import           Control.Monad.Except                (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Control.Monad.Logger                (MonadLoggerIO, logInfoN)
import           Control.Monad.Reader                (MonadReader)
import           Crypto.Hash                         (hash)
import           Data.Binary                         (encode)
import           Data.Bool                           (bool)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy                as BL (ByteString, null, toStrict, fromStrict)
import           Data.Functor                        ((<&>))
import           Data.Int                            (Int64)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map (fromSet, toList, adjust, fromList, (!?))
import           Data.Maybe                          (fromMaybe)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set (fromList, singleton, toList)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Data.Time.Units                     (Millisecond)
import           GHC.Generics                        (Generic)
import           IDIoT.Cryptonite                    (generateKeyPair, verify, getShared)
import           IDIoT.Types                         (PublicKey, PrivateProvider, Signature)
import           System.Clock                        (TimeSpec(sec))

import           Tokens.Android                      (android)
import           Tokens.Auth                         (MsgType(MsgIdKeys), auth)
import           Tokens.Bluetooth                    (ble)
import           Tokens.CCID                         (ccid)
import           Tokens.Config                       (HasLogger)
import qualified Tokens.Config                       as Config (logger)
import           Tokens.Log                          (runLoggingT)
import           Tokens.Time                         (timer, getTime)
import           Tokens.UDP                          (udp)
import           Tokens.XId                          (XId, xid)

type SyncContext r m =
  ( MonadIO m
  , MonadError Text m
  , MonadLoggerIO m
  )

data Tree
  = Branch (Map Int Tree)
  | Leaf ByteString
  deriving (Eq, Ord, Show, Generic, NFData)

sync :: SyncContext r m => m (Step (Map (Maybe XId) Tree) (Map (Maybe XId) Tree))
sync = do
  step <- liftA2 pair (liftIO $ timer (500 :: Millisecond)) $ auth idPublicKey =<< android
  let go state = Step $ \trees -> do
        let ((now0, routing0), step0) = force state

--        let send' =
--              let -- TODO: log decode errors
--                  sendOpt msgType (((sentCtr, sentVal), sentTs), ((rcvdCtr, rcvdVal), rcvdTs)) =
--                    let valOpt = case msgType of
--                          MsgIdKeys -> do
--                            -- TODO: default on decode error?
--                            keysIn <- hush . decode . BL.fromStrict $ rcvdVal
--                            let ours = Set.singleton idPublicKey
--                            let theirs = keysIn \\ ours
--                            let keysOut = ours <> theirs
--                            pure $ BL.toStrict . encode $ keysOut
--                          MsgDhKeys -> do
--                            -- TODO: default on decode error?
--                            certsIn <- hush . decode . BL.fromStrict $ rcvdVal
--                            let ours = Set.singleton ourDhCert
--                            let theirs = certsIn \\ ours
--                            let certsOut = ours <> theirs
--                            pure $ BL.toStrict . encode $ certsOut
--                    in valOpt <&> \val ->
--                      if | sentCtr == rcvdCtr && val == sentVal && val == rcvdVal ->
--                            bool Nothing (Just (succ sentCtr, val)) $ fromUnits (5 :: Second) < now0 - rcvdTs
--                         | sentCtr > rcvdCtr && val == sentVal ->
--                            bool Nothing (Just (succ sentCtr, val)) $ fromUnits (300 :: Millisecond) < now0 - sentTs
--                         | sentCtr < rcvdCtr && val == rcvdVal -> Just (rcvdCtr, val)
--                         | otherwise -> Just (succ $ max sentCtr rcvdCtr, val)
--              in Map.merge Map.dropMissing (Map.mapMaybeMissing . const $ fmap encode . (^? _head) . mapMaybe (uncurry sendOpt) . Map.toList) (Map.zipWithMatched . const $ const) send0 peers0


--        ((now1, peers), step1) <- runStep step0 ((), (NEL.head dhKeyPairs, mempty))

        pure (mempty, go ((now1. dhKeyPairs1, sessions1), step1))
  now <- liftIO getTime -- TODO: 0?
  pure . loop mempty $ go ((now), step)
