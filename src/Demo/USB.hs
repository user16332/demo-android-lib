{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokens.USB
  ( usb
  ) where

import           Control.Concurrent                  (threadDelay)
import           Control.Concurrent.MVar             (newEmptyMVar, takeMVar)
import           Control.Concurrent.Loops            (Step(Step, runStep), onCancel, loop, never,
                                                      task, pair)
import           Control.DeepSeq                     (force)
import           Control.Exception                   (SomeException)
import           Control.Exception.Safe              (isSyncException)
import           Control.Monad                       ((<=<))
import           Control.Monad.Catch                 (handleJust, uninterruptibleMask_)
import           Control.Monad.Except                (ExceptT, MonadError, runExceptT, handleError,
                                                      throwError, liftEither)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Data.Bool                           (bool)
import           Data.ByteString                     (hGetSome, hPut)
import qualified Data.ByteString.Base16              as B16 (encode)
import qualified Data.ByteString.Char8               as BC8 (unpack)
import qualified Data.ByteString.Lazy                as BL (ByteString, fromStrict, span, null, drop,
                                                      toStrict, snoc)
import           Data.Foldable                       (find, for_)
import           Data.Functor                        (void)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map (singleton, (!?))
import           Data.Maybe                          (isNothing)
import           Data.Stuffed                        (Stuffed(Stuffed), unstuff, stuff, unwrap)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Data.Time.Units                     (Millisecond)
import           System.IO                           (hClose)

import           Tokens.Android.Log                  (debug, info, err)
import           Tokens.Android.System               (JContext)
import           Tokens.Android.USB                  (Event(Event), EventDetails(USBEvent),
                                                      UsbAccessory(UsbAccessory),
                                                      registerPort, subscribeUsbAccessory,
                                                      discoverUsbAccessory)
import           Tokens.Time                         (fromUnits, timer)
import           Tokens.XId                          (XId, randomXId)

type USBContext m =
  ( MonadIO m
  , MonadError Text m
  )

logError :: a -> ExceptT Text IO a -> IO a
logError def = either (\e -> (uninterruptibleMask_ . err . T.pack . show $ e) *> pure def) pure <=< runExceptT

onError :: (MonadError e m, MonadIO m) => m b -> m a -> m a
onError action = handleError $ \e -> action *> throwError e

-- TODO: ???
throttleError :: (MonadError e m, MonadIO m) => m a -> m a
throttleError = onError . liftIO $ threadDelay 500000

handleException :: (MonadIO m, MonadError Text m) => IO a -> m a
handleException =
  liftEither <=< liftIO . handleJust (find isSyncException . Just) (\(ex :: SomeException) -> pure . Left . T.pack . show $ ex) . fmap Right

usb :: USBContext m => JContext -> m (Step (Map XId BL.ByteString) (Map XId (Maybe (), [BL.ByteString])))
usb jctx = do
  eventPort <- liftIO newEmptyMVar
  liftIO . registerPort $ eventPort
  void . subscribeUsbAccessory $ jctx
  accId <- liftIO randomXId
  let listen =
        let go = Step $ \() -> do
              eventOpt <- onCancel (pure Nothing) . fmap Just $ takeMVar eventPort
              pure (find (\case Event (USBEvent _) -> True) eventOpt, go)
        in go
  let discover = uninterruptibleMask_ . logError Nothing . discoverUsbAccessory jctx
  let send = loop Nothing . task Nothing . maybe (never Nothing) $ \(UsbAccessory handle, message) ->
        onCancel (uninterruptibleMask_ $ err "interrupted" *> pure mempty) . logError Nothing . throttleError
          . onError (liftIO . uninterruptibleMask_ $ hClose handle) . handleException $
--            debug "sending ..." *> (fmap Just . hPut handle . BL.toStrict $ message) <* debug "sent."
            fmap Just . hPut handle . BL.toStrict $ message
  let receive = loop "" . task "" . maybe (never "") $ \(UsbAccessory handle) ->
        onCancel (uninterruptibleMask_ $ err "interrupted" *> pure mempty) . logError "" . throttleError
          . onError (liftIO . uninterruptibleMask_ $ hClose handle) . handleException $ do
--            debug "reading ..." *> fmap BL.fromStrict (hGetSome handle 1024) <* debug "read."
            fmap BL.fromStrict $ hGetSome handle 1024
  step <- liftA2 pair (liftIO $ timer (500 :: Millisecond)) . pure $ pair listen . pair send $ receive
  let go state = Step $ \msgs -> do
        let ((accOpt0, last0, rcvd0), step0) = force state
--        uninterruptibleMask_ . info . T.pack $ "usb: " <> show (accOpt0, last0, rcvd0)
        let sendOpt = fmap (flip BL.snoc 0x00 . unwrap . (stuff :: BL.ByteString -> Stuffed 0)) $ msgs Map.!? accId
        ((now, (eventOpt1, (sentOpt, rcvd))), step1) <- runStep step0 ((), ((), (liftA2 (,) accOpt0 sendOpt, accOpt0)))
        (accOpt1, last1) <- maybe (fmap (,now) $ discover accOpt0) (pure . (accOpt0,)) . find ((&& isNothing eventOpt1) . (< fromUnits (500 :: Millisecond)) . (now -)) . Just $ last0
        let (messages, rcvd1) =
              let unpack (messages', rest) =
                    let (packet, next) = BL.span (/= 0x00) rest
                    in flip (bool (messages', rest)) (not . BL.null $ next) $
                      unpack (messages' <> [unstuff $ (Stuffed packet :: Stuffed 0)], BL.drop 1 next)
              in unpack ([], rcvd0 <> rcvd)
--        for_ messages $ debug . T.pack . BC8.unpack . B16.encode . BL.toStrict
        pure (Map.singleton accId (sentOpt, messages), go ((accOpt1, last1, rcvd1), step1))
  pure . loop mempty . go $ ((Nothing, 0, ""), step)
