{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE OverloadedStrings          #-}

module Demo.Android.Log
  ( debug
  , info
  , warn
  , err
  , runLoggingT
  ) where

import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Control.Monad.Logger                (LogLevel(LevelDebug, LevelInfo, LevelWarn,
                                                      LevelError, LevelOther), LoggingT,
                                                      fromLogStr)
import qualified Control.Monad.Logger                as MonadLogger (runLoggingT)
import           Data.Text                           (Text)
import qualified Data.Text.Encoding                  as T (decodeUtf8)
import qualified Data.Text.Foreign                   as T (withCString)
import           Foreign.C.String                    (CString)
import           Foreign.C.Types                     (CInt(CInt))

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
  T.withCString "Demo" $ \tag ->
    T.withCString msg $ _log (fromIntegral . fromEnum $ prio) tag

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
