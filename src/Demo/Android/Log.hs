{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tokens.Android.Log
  ( debug
  , info
  , warn
  , err
  , runLoggingT
  ) where

import           Control.Concurrent.MSem             (MSem,)
import qualified Control.Concurrent.MSem             as MSem (new, with)
import           Control.Monad.Catch                 (bracket)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Control.Monad.Logger                (LogLevel(LevelDebug, LevelInfo, LevelWarn,
                                                      LevelError, LevelOther), LoggingT,
                                                      fromLogStr)
import qualified Control.Monad.Logger                as MonadLogger (runLoggingT)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack, singleton)
import qualified Data.Text.Encoding                  as T (encodeUtf8, decodeUtf8)
import qualified Data.Text.Foreign                   as T (withCString)
import           Data.Time                           (UTCTime, getCurrentTime)
import           Data.Time.Format                    (defaultTimeLocale, formatTime)
import           Foreign.C.String                    (CString)
import           Foreign.C.Types                     (CInt(CInt))
import           Network.Socket                      (SockAddr(SockAddrInet), Family(AF_INET),
                                                      SocketType(Datagram), defaultProtocol,
                                                      socket, close, tupleToHostAddress, bind)
import           Network.Socket.ByteString           (sendAllTo)
import           System.IO.Unsafe                    (unsafePerformIO)

data LogPriority
  = PrioUnknown
  | PrioDefault
  | PrioVerbose
  | PrioDebug
  | PrioInfo
  | PrioWarn
  | PrioError
  | PrioFatal
  | PrioSilent
  deriving (Eq, Ord, Show, Bounded, Enum)

prioChar :: LogPriority -> Char
prioChar PrioUnknown = '?'
prioChar PrioDefault = '?'
prioChar PrioVerbose = 'V'
prioChar PrioDebug = 'D'
prioChar PrioInfo = 'I'
prioChar PrioWarn = 'W'
prioChar PrioError = 'E'
prioChar PrioFatal = 'F'
prioChar PrioSilent = 'X'

foreign import capi "__android_log_print" _log :: CInt -> CString -> CString -> IO ()

logAndroid :: MonadIO m => LogPriority -> Text -> m ()
logAndroid prio msg = liftIO $ do
  T.withCString "Tokens" $ \tag ->
    T.withCString msg $ _log (fromIntegral . fromEnum $ prio) tag
--  logUDP prio msg

bindSem :: MSem Int
bindSem = unsafePerformIO $ MSem.new 1

logUDP :: MonadIO m => LogPriority -> Text -> m ()
logUDP prio msg = liftIO $ do
  utc <- getCurrentTime
  let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%03Q" utc
  let raw = T.encodeUtf8 $ ts <> " " <> T.singleton (prioChar prio) <> " " <> msg <> "\n"
  MSem.with bindSem . bracket (socket AF_INET Datagram defaultProtocol) close $ \s -> do
    bind s $ SockAddrInet 6789 0
    sendAllTo s raw . SockAddrInet 6789 $ tupleToHostAddress (192, 168, 1, 3)

debug :: MonadIO m => Text -> m ()
debug = logAndroid PrioDebug

info :: MonadIO m => Text -> m ()
info = logAndroid PrioInfo

warn :: MonadIO m => Text -> m ()
warn = logAndroid PrioWarn

err :: MonadIO m => Text -> m ()
err = logAndroid PrioError

toAndroid :: LogLevel -> LogPriority
toAndroid (LevelOther "Verbose") = PrioVerbose
toAndroid LevelDebug = PrioDebug
toAndroid LevelInfo  = PrioInfo
toAndroid LevelWarn  = PrioWarn
toAndroid LevelError = PrioError
toAndroid (LevelOther "Fatal") = PrioFatal
toAndroid _ = PrioUnknown

runLoggingT :: LoggingT m a -> m a
runLoggingT = flip MonadLogger.runLoggingT $ \_ _ level msg ->
  liftIO . logAndroid (toAndroid level) . T.decodeUtf8 . fromLogStr $ msg
