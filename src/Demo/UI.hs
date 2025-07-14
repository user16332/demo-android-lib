{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Tokens.UI
  ( ui
  ) where

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
import           Data.Monoid                         (Endo(Endo, appEndo))
import           Data.Foldable                       (for_, find)
import           Data.Function                       ((&))
import           Data.Functor                        (void)
import           Data.Maybe                          (fromMaybe, catMaybes)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (pack)
import           Data.Time.Units                     (Millisecond)
import           Data.Traversable                    (for)
import           Foreign.JNI                         (JVMException, showException,
                                                      runInAttachedThread, isSameObject)
import           GHC.Generics                        (Generic)
import           Language.Java                       (J(J))
import           System.Clock                        (TimeSpec)

import           Tokens.Events                       (Event(Event), EventPattern(..), Ext(Ext), coi, apply)
import           Tokens.Time                         (timer, getTime)
import           Tokens.Android.Log                  (info, debug, err)
import           Tokens.Android.UI                   (Node(Node), JContext, EventDetails(ActivityEvent),
                                                      View(Leaf), Ctrl(CtrlText), TextView(TextView), JActivity,
                                                      ActivityEventType(ActivityCreate),
                                                      registerPorts, notifyUIUpdate)
import qualified Tokens.Android.UI                   as UI (Event(Event))
import           Tokens.XId                          (XId)
import qualified Tokens.XId                          as XId (fromByteString)

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

mainActivityEventId :: XId
mainActivityEventId = fromMaybe undefined . XId.fromByteString . BS.pack $ [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]

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
  step <- liftA2 pair (liftIO $ timer (1000 :: Millisecond)) . pure $ uiEvents uiEventPort
  let go state = Step $ \() -> do
        let (app0, ui0, step0) = force state
--        uninterruptibleMask_ . debug . T.pack . show $ app0
        let ext = forAppState app0
        events <- uninterruptibleMask_ . fmap catMaybes . for (coi ext) $ \part -> either (\e -> err e *> pure Nothing) pure <=< runExceptT $
          case part of
            _ -> pure Nothing

        (events', step1) <- flip (bool $ pure (events, step0)) (null events) $ do
          ((now, uiEventOpt), step1) <- runStep step0 ((), ())
          let tsEvents = pure . Event EventTimestamp $ toDyn now
          let uiEvents = flip foldMap uiEventOpt $ \(UI.Event xid details) -> pure . Event (EventUI xid) $ toDyn details
          pure (tsEvents <> uiEvents, step1)

        let app1 = (appEndo $ ext `apply` events') app0

        let ui1 = Node def (Leaf (CtrlText TextView "boo"))

--        let ui1 = flip (maybe ui) eventOpt $ \case
--              Event _ ActionEvent -> ui & ndRegion . paChildren . ix "welcome" . _2 . ndRegion . lfCtrl . lbText .~ "42!!"
        for_ (app1 ^. appMainActivityOpt) $ \jactivity -> do
          isSameActivity <- maybe (pure False) (uninterruptibleMask_ . either (\e -> err e *> pure False) pure <=< runExceptT . handleException . runInBoundThread . runInAttachedThread . isSameObject jactivity) $ app0 ^. appMainActivityOpt
          when (not isSameActivity || ui1 /= ui0) $ do
            void $ tryTakeMVar uiUpdatePort
            putMVar uiUpdatePort ui1
            notifyUIUpdate jactivity
        pure ((), go (app1, ui1, step1))
  now <- liftIO getTime
  pure . loop () $ go (AppState now Nothing, Node def (Leaf (CtrlText TextView "")), step) -- initial
