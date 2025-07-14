{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokens.Auth
  ( Message(MsgIdKey)
  , Signer
  , auth
  ) where

import           Control.Concurrent.Loops            (Step(Step), loop, pair, runStep)
import           Control.DeepSeq                     (NFData, force)
import           Control.Error.Util                  (hush)
import           Control.Lens                        ((%~), _1, _2, (.~), (^?), (^.), _head, to)
import           Control.Lens.TH                     (makeLenses)
import           Control.Monad                       (guard, unless, (<=<))
import           Control.Monad.Catch                 (uninterruptibleMask_)
import           Control.Monad.Except                (MonadError, throwError)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Control.Monad.Logger                (MonadLoggerIO, logInfoN)
import           Crypto.Hash                         (hash)
import           Crypto.Random.Types                 (MonadRandom(getRandomBytes))
import           Data.Binary                         (Binary, decodeOrFail, encode)
import           Data.Bool                           (bool)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy                as BL (ByteString, null, toStrict, fromStrict)
import           Data.Default                        (Default(def))
import           Data.Foldable                       (find, foldl')
import           Data.Function                       (on)
import           Data.Functor                        ((<&>))
import           Data.Int                            (Int64)
import           Data.List.NonEmpty                  (NonEmpty, (:|))
import qualified Data.List.NonEmpty                  as NEL (head, last)
import           Data.Monoid                         (Endo(appEndo))
import           Data.Maybe                          (mapMaybe, fromMaybe)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map (fromSet, toList, adjust, (!?), insertWith,
                                                      member)
import qualified Data.Map.Merge.Strict               as Map (merge, mapMaybeMissing, mapMissing,
                                                      zipWithMatched, dropMissing, zipWithMaybeMatched,
                                                      preserveMissing)
import           Data.Maybe                          (isJust)
import           Data.Set                            (Set, (\\))
import qualified Data.Set                            as Set (map, filter, fromList, singleton, delete)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Data.Time.Units                     (Millisecond, Second)
import           GHC.Generics                        (Generic)
import           IDIoT.Cryptonite                    (verify, sign)
import           IDIoT.Types                         (PublicKey, Signature)
import           System.Clock                        (TimeSpec)
import           Witherable                          (mapMaybe)

import           Tokens.Crypto                       (SymKey, HMAC, getShared, encryptAndMac, macAndDecrypt,
                                                      isValid)
import           Tokens.Time                         (timer, getTime, fromUnits)
import           Tokens.XId                          (XId, xid)

type AuthContext m =
  ( MonadIO m
  , MonadError Text m
  , MonadLoggerIO m
  )

type PeerId = XId

type BranchId = XId

type Serial = Int32

type Coord = Int32

data Message
  = MsgBranch (PeerId, BranchId) BranchMsg -- TODO: peer ids optl.?
  deriving (Eq, Ord, Show, Generic, NFData, Binary)

-- target sources int.sig ext.sig
data Fork = Fork PublicKey (Set BranchId) Signature Signature
  deriving (Eq, Ord, Show, Generic, NFData, Binary)

data BranchMsg
  = MsgHeartbeat Serial
  | MsgPeerKey Serial (Maybe PublicKey)
  | MsgFork Serial (Maybe Fork) -- TODO: encrypted?
  | MsgValue (PeerId, BranchId) ValueMsg -- TODO: peer ids optl.? serial unencrypted?
  deriving (Eq, Ord, Show, Generic, NFData, Binary)

data ValueMsg =
  = MsgValue (IV, ByteString) HMAC
  deriving (Eq, Ord, Show, Generic, NFData, Binary)

data Value = Value Coord Serial ByteString
  deriving (Eq, Ord, Show, Generic, NFData, Binary)

decode :: (MonadError Text m, Binary a) => BL.ByteString -> m a
decode bs = do
  (rest, _, a) <- either (\(_, _, e) -> throwError $ T.pack e) pure . decodeOrFail $ bs
  unless (BL.null rest) $ throwError "unconsumed input"
  pure a

type Signer = Digest SHA256 -> ExceptT Text IO Signature

data Branch =
  { _brKeyId :: XId -- TODO: merkle hash?
  , _brSymKey :: SymKey
  } deriving (Eq, Ord, Show, Generic, NFData)
makeLenses ''Branch

-- dummy
--instance Default Branch where
--  def = Branch def def mempty
--
--instance Default ByteString32 where
--  def = either undefined id . refine $ BS.replicate 32 0x00

-- dummy
--instance Default PublicKey where
--  def = PublicKey def def

data Value a = Value
  { _vCtr :: Int32
  , _vTs :: TimeSpec
  , _vVal :: a
  } deriving (Eq, Ord, Show, Generic, NFData)
makeLenses ''Value

data Sync a = Sync
  { _scSent :: Value a
  , _scRcvd :: Value a
  } deriving (Eq, Ord, Show, Generic, NFData)
makeLenses ''Sync

data Routing = Routing
  { _rtOurBranches :: Table (BranchId, BranchId) -- (our branch ID from, our branch ID to) -> unit

  , _rtConnBranch -- (conn, our branch ID) -> times/serial + unit
  , _rtConnOurPeerKey -- (conn, our branch ID) -> times/serial + boolean (ref our peer key)
  , _rtConnOurBranchKey -- (conn, our branch ID from, our branch ID to) -> times/serial + maybe (branch to key)

    -- ideal:
    -- (conn, their peer ID, their peer nonce) -> times/serial + unit
    -- (their peer ID) -> peer key
    -- (their branch ID to) -> branch key

  , _rtConnTheirBranch -- (conn, their peer ID, their branch ID) -> times/serial + unit // ideal: (their peer ID, their branch ID) -> times/serial + unit
  , _rtConnTheirPeerKey -- (conn, their peer ID, their branch ID) -> times/serial + maybe (peer key)
           -- // ideal: (their peer ID, their branch ID) -> times/serial + boolean (ref peer key)
           --    event more ideal: (their peer ID, their peer nonce) -> times/serial + boolean (ref peer key)
  , _rtConnTheirFork -- (conn, their peer ID, their branch ID from, their branch ID to) -> times/serial + maybe (branch key)
           -- // ideal: (their branch ID from, their branch ID to) -> times/serial + unit
           --           (their branch ID to) -> times/serial + boolean (ref branch key)

  , _rtSymmKey -- (their peer ID, their branch ID, our branch ID) -> symmetric key
  , _rtValues -- (their peer ID, their branch ID, our branch ID, coord) -> times/serial + value
  } deriving (Eq, Ord, Show, Generic, NFData)
makeLenses ''Routing

auth :: AuthContext m => (PublicKey, Signer) -> Step (Map XId BL.ByteString) (Map XId (Maybe (), Set BL.ByteString)) -> m (Step (Map XId BL.ByteString) (Map XId BL.ByteString))
auth (idPublicKey, idSign) transport = do
  step <- liftA2 pair (liftIO $ timer (200 :: Millisecond)) $ pure transport
  let go state = Step $ \msgs -> do
        -- TODO: purge orphans quickly
        let ((now0, routing0, send0), step0) = force state
        uninterruptibleMask_ . logInfoN . T.pack . show $ (now0, routing0, send0)

        (dhKeyPairs1, peers') <-
          let period = (`mod` 30) . sec $ now0 -- TODO: artificial variability
          in flip (flip maybe . pure . (, peers0)) (find ((period <=) . (^. dhPeriod)) . Just $ dhKeyPairs0) .
              either undefined pure <=< runExceptT $ do -- TODO: log error
                -- expand
                (newPub, newPriv) <- liftIO generateKeyPair
                ((prevPub, _), _) <- dhKeyPairs0 ^. dhPath . to NEL.head
                sig <- idSign . hash . BL.toStrict . encode $ (newPub, prevPub)
                invSig <- sign newPriv . hash . BL.toStrict . encode $ idKey
                let path' = dhKeyPairs0 ^. dhPath <> [((newPub, newPriv), sig, invSig)]
                let peers' = peers0 <&> \peer ->
                      peer & peBranches %~ (\l -> (<> [l])) $ peer ^. peDhKeys <&> \theirPub ->
                        let symKey = getShared newPriv theirPub
                        in Branch (xid . BL.toStrict . encode $ symKey) symkey mempty
                -- TODO: assert consistency
                -- pendings to commitments

                -- contract
                let (path'', peers'') =
                      let go (path, peers) =
                            case path of
                              kp0 :| (kp1 : kpn) | length kpn >= 1 ->
                                let peers' = peers <$> peBranches .~ \case
                                      (Branch _ _ vals0) :| (br1 : brn) | length brn >= 1 ->
                                        (br1 & brValues %~ (fold vals0 <>)) :| brn
                                      _ -> undefined -- inconsistent
                                in go (kp1 :| kpn, peers')
                              _ -> (path, peers)
                      in go (path', peers')
                -- TODO: assert consistency
                pure (DhKeyPairs period path'', peers'')
        uninterruptibleMask_ . logInfoN . T.pack . show $ dhKeyPairs1

        -- trim theirDhCerts?
        -- trim peers

        -- send keys
        -- send dh's
        -- send payloads

        send' <- peers' <&> \peer ->

        ((now1, doneRcvd), step1) <- runStep step0 ((), send')

        -- TODO: keep track of *authenticated* connections (received mac'd values) to prioritize sending values to


        let rcvd = mapMaybe (hush . decode) . fmap snd $ doneRcvd



        -- TODO: handle errors
        -- TODO: decode into map + parallel map merge

        let applyMsg = appEndo . flip foldMap pending0 $ \case
              msg@(MsgIdKey idKey) -> Endo $ \(peers, symKeys, pending) ->
                let peer = def ^. peIdKey .~ idKey
                    peerId = xid . BL.toStrict . encode $ idKey
                in (Map.insertWith (flip const) peerId peer, symKeys, Set.delete msg pending)
                -- TODO: symkeys?

                -- rcvd timestamps?
              msg@(MsgDhKey (ourId, theirId) (dhKeys@(newKey, prevKey), sig, invSig)) -> Endo $ \(peers, symKeys, pending) ->
                -- TODO: null peer ids + encrypted?
                flip (bool (peers, symKeys, pending)) (((ourId ==) xid . BL.toStrict . encode $ idPublicKey) && Map.member theirId peers && peer ^. peDhKeys . to NEL.last == prevKey) $
                  let peer' = \peer -> -- TODO: Endo?
                        fromMaybe peer $ do
                          -- TODO: log pubKey error
                          guard . isValid $ newKey
                          guard . verify (peer ^. peIdKey) sig . hash . BL.toStrict . encode $ dhKeys
                          guard . verify newKey invSig . hash . BL.toStrict . encode $ peer ^. peIdKey
                          -- expand
                          let peer' = peer & peBranches %~ \brs -> NEL.zip (dhKeyPairs1 ^. dhPath, brs) <&> \(((_, privKey), _), subbrs) ->
                                let symKey = getShared privKey newKey
                                in (subbrs <>) . pure $ Branch (xid . BL.toStrict . encode $ symKey) symkey mempty
                          -- TODO: assert consistency

                          -- contract
                          let peer'' =
                                let go peer =
                                      case peer & peDhKeys of
                                        k0 :| (k1 : kn) | length kn >= 1 ->
                                          let brs' = peer & peBranches %~ fmap $ \case
                                                (Branch _ _ vals0) :| (sb1 : sbn) | length sbn >= 1 ->
                                                  (sb1 ^. brValues %~ (vals0 <>)) :| sbn
                                                _ -> undefined
                                          in peer & peDhKeys .~ (k1 :| kn) & peBranches .~ brs'
                                        _ -> peer
                                in go peer'
                          -- TODO: assert consistency
                    peers' = Map.adjust peer' theirId peers
                    symKeys' = flip Map.foldMapWithKey peers' $ \theirId -> foldMap (flip Map.singleton theirId . (^. brSymKey)) . (^. peBranches)
                  in (peers', symKeys', Set.delete msg pending)

                -- rcvd timestamps?
              msg@(MsgPayload keyId (encrypted, mac)) -> Endo $ \(peers, symKeys, pending) ->
                flip (maybe (peers, symKeys, pending)) (symKeys Map.!? keyId) $ \peerId ->
                  let peer' = \peer ->
                        peer & peBranches %~ fmap . fmap $ \br -> -- TODO: Endo?
                          fromMaybe br $ do
                            guard $ br ^. brKeyId == keyId
                            -- TODO: log decrypt error
                            payload <- macAndDecrypt (br ^. brSymKey) encrypted
                            pure $ br & vals %~ (<> payload)
                  in (Map.adjust peer' peerId peers, symKeys, Set.delete msg pending)
                -- rcvd timestamps?

        -- TODO: handle decode errors errors
        let (peers1, symKeys1, pending1) =
              fix applyMsg (peers', symKeys, mapMaybe (hush . decode) rcvd <> pending)

        let send1 =
        let result =
        pure (result, go ((now1, dhKeyPairs1, peers1, symKeys1, pending1, send1), step1))
  now <- liftIO getTime -- TODO: 0?
  pure . loop mempty $ go ((now, mempty, mempty, mempty, mempty, mempty), step)
