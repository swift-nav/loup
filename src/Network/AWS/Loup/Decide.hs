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
import Data.Conduit.List        hiding (foldM)
import Data.UUID
import Data.UUID.V4
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

scheduleActivity :: UUID -> ActivityType -> TaskList -> Maybe Text -> Decision
scheduleActivity uid activity list input = do
  let satda = scheduleActivityTaskDecisionAttributes activity (toText uid)
        & satdaTaskList .~ return list
        & satdaInput    .~ input
  set dScheduleActivityTaskDecisionAttributes (return satda) $ decision ScheduleActivityTask

completeActivity :: Decision
completeActivity = do
  let cweda = completeWorkflowExecutionDecisionAttributes
  set dCompleteWorkflowExecutionDecisionAttributes (return cweda) $ decision CompleteWorkflowExecution

cancelActivity :: Decision
cancelActivity = do
  let cweda = cancelWorkflowExecutionDecisionAttributes
  set dCancelWorkflowExecutionDecisionAttributes (return cweda) $ decision CancelWorkflowExecution

requestCancel :: UUID -> Decision
requestCancel uid = do
  let rcatda = requestCancelActivityTaskDecisionAttributes (toText uid)
  set dRequestCancelActivityTaskDecisionAttributes (return rcatda) $ decision RequestCancelActivityTask

foldEvents :: MonadDecisionCtx c m => a -> (a -> HistoryEvent -> m a) -> m a
foldEvents base action = do
  events <- view dcEvents
  foldM action base events

findEvent :: MonadDecisionCtx c m => EventType -> m (Maybe HistoryEvent)
findEvent eventType = do
  let f ds e = return $ bool ds (Just e) $ e ^. heEventType == eventType
  foldEvents Nothing f

begin :: MonadDecisionCtx c m => HistoryEvent -> m [Decision]
begin event = do
  traceInfo "begin" mempty
  uid  <- liftIO nextRandom
  task <- view pActivityTask <$> view dcPlan
  let input = join $ view weseaInput <$> event ^. heWorkflowExecutionStartedEventAttributes
  return [ scheduleActivity uid (task ^. tActivityType) (task ^. tTaskList) input ]

cancel :: MonadDecisionCtx c m => m [Decision]
cancel = do
  traceInfo "cancel" mempty
  event <- findEvent ActivityTaskScheduled
  let uid = join $ fromText . view atseaActivityId <$> join (view heActivityTaskScheduledEventAttributes <$> event)
  return [ maybe cancelActivity requestCancel uid ]

completed :: MonadDecisionCtx c m => m [Decision]
completed = do
  traceInfo "completed" mempty
  return [ completeActivity ]

canceled :: MonadDecisionCtx c m => m [Decision]
canceled = do
  traceInfo "canceled" mempty
  return [ completeActivity ]

timedout :: MonadDecisionCtx c m => m [Decision]
timedout = do
  traceInfo "timedout" mempty
  uid   <- liftIO nextRandom
  task  <- view pActivityTask <$> view dcPlan
  event <- findEvent WorkflowExecutionStarted
  let input = join $ view weseaInput <$> join (view heWorkflowExecutionStartedEventAttributes <$> event)
  return [ scheduleActivity uid (task ^. tActivityType) (task ^. tTaskList) input ]

failed :: MonadDecisionCtx c m => m [Decision]
failed = do
  traceInfo "failed" mempty
  return [ cancelActivity ]

-- | Schedule decision based on history events.
--
schedule :: MonadDecisionCtx c m => m [Decision]
schedule = do
  traceInfo "schedule" mempty
  let f ds e
        | e ^. heEventType == WorkflowExecutionStarted         = begin e
        | e ^. heEventType == WorkflowExecutionCancelRequested = cancel
        | e ^. heEventType == ActivityTaskCompleted            = completed
        | e ^. heEventType == ActivityTaskCanceled             = canceled
        | e ^. heEventType == ActivityTaskTimedOut             = timedout
        | e ^. heEventType == RequestCancelActivityTaskFailed  = failed
        | otherwise                                            = return ds
  foldEvents mempty f

-- | Decider logic - poll for decisions, make decisions.
--
decide :: MonadAmazonCtx c m => Text -> Plan -> m ()
decide domain plan =
  preAmazonCtx [ "label" .= LabelDecide, "domain" .= domain, "plan" .= plan ] $ do
    traceInfo "poll" mempty
    (token, events) <- pollDecision domain (plan ^. pDecisionTask ^. tTaskList)
    maybe_ token $ \token' -> do
      traceInfo "start" mempty
      runDecisionCtx plan events $ do
        decisions <- schedule
        completeDecision token' decisions
      traceInfo "finish" mempty

-- | Run decider from main with configuration.
--
decideMain :: MonadControl m => Text -> FilePath -> m ()
decideMain domain file =
  runResourceT $ runCtx $ runStatsCtx $ runAmazonCtx $ do
    plans <- liftIO $ join . maybeToList <$> decodeFile file
    runConcurrent $ forever . decide domain <$> plans
