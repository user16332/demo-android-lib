{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Demo.UI
  ( ui
  ) where

import           Control.Applicative                 ((<|>))
import           Control.Concurrent                  (runInBoundThread)
import           Control.Concurrent.Loops            (Step(Step, runStep), onCancel, loop, pair)
import           Control.Concurrent.MVar             (MVar, tryTakeMVar, putMVar, takeMVar, newEmptyMVar)
import           Control.DeepSeq                     (NFData, force)
import           Control.Exception                   (SomeException)
import           Control.Exception.Safe              (isSyncException)
import           Control.Lens                        ((^.), (.~))
import           Control.Lens.TH                     (makeLenses)
import           Control.Monad                       (when, (<=<))
import           Control.Monad.Catch                 (handleJust, handle, uninterruptibleMask_)
import           Control.Monad.Except                (MonadError, liftEither, runExceptT)
import           Control.Monad.IO.Class              (MonadIO(liftIO))
import           Control.Monad.Reader                (MonadReader)
import           Data.Bool                           (bool)
import qualified Data.ByteString                     as BS (pack)
import           Data.Default                        (Default(def))
import           Data.Dynamic                        (toDyn, fromDyn)
import           Data.Foldable                       (for_, find)
import           Data.Function                       ((&))
import           Data.Functor                        (void)
import           Data.Int                            (Int32)
import           Data.Maybe                          (fromMaybe, catMaybes, isNothing)
import           Data.Monoid                         (Endo(Endo, appEndo))
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Data.Time.Clock                     (getCurrentTime)
import           Data.Time.Format                    (formatTime, defaultTimeLocale)
import           Data.Time.Units                     (Millisecond)
import           Data.Traversable                    (for)
import           Foreign.JNI                         (JVMException, showException,
                                                      runInAttachedThread, isSameObject)
import           GHC.Generics                        (Generic)
import           Language.Java                       (J(J))
import           System.Clock                        (TimeSpec)

import           Demo.Events                         (Event(Event), EventPattern(..), Ext(Ext), coi, apply)
import           Demo.Time                           (timer, getTime)
import           Demo.Android.Log                    (info, debug, err)
import           Demo.Android.UI                     (JContext, EventDetails(ActivityEvent, MethodInvocation),
                                                      JActivity, ActivityEventType(ActivityCreate), beep,
                                                      registerPorts, notifyUIUpdate, buttonClickEventId)
import qualified Demo.Android.UI                     as UI (Event(Event))

handleException :: (MonadIO m, MonadError Text m) => IO a -> m a
handleException =
  liftEither <=< liftIO . handleJust (find isSyncException . Just) (\(ex :: SomeException) -> pure . Left . T.pack . show $ ex)
    . handle (\(ex :: JVMException) -> fmap Left $ showException ex) . fmap Right

type UIContext r m =
  ( MonadIO m
  , MonadReader r m
  , MonadError Text m
  )

data AppState = AppState
  { _appTimestamp :: TimeSpec
  , _appMainActivityOpt :: Maybe JActivity
  } deriving (Eq, Show, Generic, NFData)
makeLenses ''AppState

mainActivityEventId :: Int32
mainActivityEventId = 1000

forAppState :: AppState -> Ext AppState
forAppState app = Ext $
  [ (EventTimestamp, Endo . (appTimestamp .~) . flip fromDyn undefined)
  -- TODO: log error on mismatch
  , (EventUI mainActivityEventId, (\case (ActivityEvent jactivity ActivityCreate) -> Endo $ \app1 -> app1 & appMainActivityOpt .~ Just jactivity; _ -> mempty) . flip fromDyn undefined)
--  -- TODO: check same object?
--  , (EventUI mainActivityEventId, (\case (ActivityEvent _ ActivityStop) -> Endo $ \app1 -> app1 & appMainActivityOpt %~ const Nothing; _ -> mempty) . flip fromDyn undefined)
  ]

uiEvents :: MVar UI.Event -> Step () (Maybe UI.Event)
uiEvents uiEventPort =
  let go state = Step $ \() -> do
        let log0 = force state
--        uninterruptibleMask_ . debug . T.intercalate "\n" $ log0
        eventOpt <- onCancel (pure Nothing) . fmap Just . takeMVar $ uiEventPort
--        uninterruptibleMask_ . for_ eventOpt $ debug . T.pack . show
        let log1 = maybe log0 (take 10 . (<> log0) .  pure . T.pack . show) $ eventOpt
        pure (eventOpt, go log1)
  in go mempty

ui :: UIContext r m => JContext -> m (Step () ())
ui jctx = do
  (uiUpdatePort, uiEventPort) <- handleException $ do
    uiUpdatePort <- newEmptyMVar -- TODO: queue??
    uiEventPort <- newEmptyMVar -- TODO: queue??
    registerPorts uiUpdatePort uiEventPort
    pure (uiUpdatePort, uiEventPort)
  step <- liftA2 pair (liftIO $ timer (100 :: Millisecond)) . pure $ uiEvents uiEventPort
  let go state = Step $ \() -> do
        let (activityOpt0, ui0, step0) = force state
--        uninterruptibleMask_ . debug . T.pack . show $ app0
        ((_, uiEventOpt), step1) <- runStep step0 ((), ())
        utc <- uninterruptibleMask_ getCurrentTime
        let activityOpt1 =
              (uiEventOpt >>= \case UI.Event eventId (ActivityEvent activity ActivityCreate) | eventId == mainActivityEventId -> Just activity; _ -> Nothing) <|> activityOpt0
        for_ uiEventOpt $ \case UI.Event eventId (MethodInvocation _ _) | eventId == buttonClickEventId -> beep; _ -> pure ()
        let ui1 = T.pack $ formatTime defaultTimeLocale "%H:%M:%S" utc
        for_ activityOpt1 $ \activity ->
          when (ui1 /= ui0 || isNothing activityOpt0) $ do
            void $ tryTakeMVar uiUpdatePort
            putMVar uiUpdatePort ui1
            notifyUIUpdate activity
        pure ((), go (activityOpt1, ui1, step1))
  now <- liftIO getTime
  pure . loop () $ go (Nothing, "barbar", step) -- initial
