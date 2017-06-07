{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Decider logic.
--
module Network.AWS.Loup.Decide
  ( decide
  , decideMain
  ) where

import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Conduit.List hiding (foldM)
import Data.Yaml
import Network.AWS.Loup.Ctx
import Network.AWS.Loup.Prelude
import Network.AWS.Loup.Types
import Network.AWS.SWF

-- | Poll for decision.
--
pollDecision :: MonadAmazonCtx c m => Text -> TaskList -> m (Maybe Text, [HistoryEvent])
pollDecision domain list = do
  pfdtrs <- paginate (pollForDecisionTask domain list) $$ consume
  return (join $ headMay $ view pfdtrsTaskToken <$> pfdtrs, reverse $ join $ view pfdtrsEvents <$> pfdtrs)

-- | Successful decision completion.
--
completeDecision :: MonadAmazonCtx c m => Text -> [Decision] -> m ()
completeDecision token decisions =
  void $ send $ set rdtcDecisions decisions $ respondDecisionTaskCompleted token

foldEvents :: MonadDecisionCtx c m => a -> (a -> HistoryEvent -> m a) -> m a
foldEvents base action = do
  events <- view dcEvents
  foldM action base events

begin :: MonadDecisionCtx c m => HistoryEvent -> m [Decision]
begin _event = undefined

completed :: MonadDecisionCtx c m => HistoryEvent -> m [Decision]
completed _event = undefined

timedout :: MonadDecisionCtx c m => HistoryEvent -> m [Decision]
timedout _event = undefined

cancel :: MonadDecisionCtx c m => HistoryEvent -> m [Decision]
cancel _event = undefined

canceled :: MonadDecisionCtx c m => HistoryEvent -> m [Decision]
canceled _event = undefined

failed :: MonadDecisionCtx c m => HistoryEvent -> m [Decision]
failed _event = undefined

-- | Schedule decision based on history events.
--
schedule :: MonadDecisionCtx c m => m [Decision]
schedule =
  foldEvents mempty f
  where
    f ds e
      | e ^. heEventType == WorkflowExecutionStarted         = begin e
      | e ^. heEventType == ActivityTaskCompleted            = completed e
      | e ^. heEventType == ActivityTaskTimedOut             = timedout e
      | e ^. heEventType == WorkflowExecutionCancelRequested = cancel e
      | e ^. heEventType == ActivityTaskCanceled             = canceled e
      | e ^. heEventType == RequestCancelActivityTaskFailed  = failed e
      | otherwise                                            = return ds

-- | Decider logic - poll for decisions, make decisions.
--
decide :: MonadAmazonCtx c m => Text -> Plan -> m ()
decide domain plan = do
  (token, events) <- pollDecision domain (plan ^. pDecisionTask ^. tTaskList)
  maybe_ token $ \token' ->
    runDecisionCtx plan events $ do
      decisions <- schedule
      completeDecision token' decisions

-- | Run decider from main with configuration.
--
decideMain :: MonadControl m => Text -> FilePath -> m ()
decideMain domain file =
  runResourceT $ runCtx $ runStatsCtx $ runAmazonCtx $ do
    plans <- liftIO $ join . maybeToList <$> decodeFile file
    runConcurrent $ forever . decide domain <$> plans
