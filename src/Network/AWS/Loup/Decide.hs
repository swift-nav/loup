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
  pfdtrs <- pages $ pollForDecisionTask domain list
  return (join $ headMay $ view pfdtrsTaskToken <$> pfdtrs, reverse $ join $ view pfdtrsEvents <$> pfdtrs)

-- | Successful decision completion.
--
completeDecision :: MonadAmazonCtx c m => Text -> [Decision] -> m ()
completeDecision token decisions =
  void $ send $ respondDecisionTaskCompleted token
    & rdtcDecisions .~ decisions

-- | Schedule activity decision.
--
scheduleActivity :: UUID -> ActivityType -> TaskList -> Maybe Text -> Decision
scheduleActivity uid activity list input = do
  let satda = scheduleActivityTaskDecisionAttributes activity (toText uid)
        & satdaTaskList .~ return list
        & satdaInput    .~ input
  decision ScheduleActivityTask
    & dScheduleActivityTaskDecisionAttributes .~ return satda

-- | Complete activity decision.
--
completeActivity :: Decision
completeActivity = do
  let cweda = completeWorkflowExecutionDecisionAttributes
  decision CompleteWorkflowExecution
    & dCompleteWorkflowExecutionDecisionAttributes .~ return cweda

-- | Cancel activity decision.
--
cancelActivity :: Decision
cancelActivity = do
  let cweda = cancelWorkflowExecutionDecisionAttributes
  decision CancelWorkflowExecution
    & dCancelWorkflowExecutionDecisionAttributes .~ return cweda

-- | Request to cancel activity decision.
--
requestCancel :: UUID -> Decision
requestCancel uid = do
  let rcatda = requestCancelActivityTaskDecisionAttributes (toText uid)
  decision RequestCancelActivityTask
    & dRequestCancelActivityTaskDecisionAttributes .~ return rcatda

-- | Find a matching event type.
--
findEvent :: MonadDecisionCtx c m => EventType -> m (Maybe HistoryEvent)
findEvent eventType = do
  let f []     = return Nothing
      f (e:es) = bool (f es) (return $ Just e) $ e ^. heEventType == eventType
  events <- view dcEvents
  f events

-- | Begin workflow step.
--
begin :: MonadDecisionCtx c m => HistoryEvent -> m [Decision]
begin event = do
  traceInfo "begin" mempty
  uid  <- liftIO nextRandom
  task <- view pActivityTask <$> view dcPlan
  let input = join $ view weseaInput <$> event ^. heWorkflowExecutionStartedEventAttributes
  return [ scheduleActivity uid (task ^. tActivityType) (task ^. tTaskList) input ]

-- | Cancel workflow step.
--
cancel :: MonadDecisionCtx c m => m [Decision]
cancel = do
  traceInfo "cancel" mempty
  event <- findEvent ActivityTaskScheduled
  let uid = join $ fromText . view atseaActivityId <$> join (view heActivityTaskScheduledEventAttributes <$> event)
  return [ maybe cancelActivity requestCancel uid ]

-- | Completed workflow step.
--
completed :: MonadDecisionCtx c m => m [Decision]
completed = do
  traceInfo "completed" mempty
  return [ completeActivity ]

-- | Restart workflow step.
--
restart :: MonadDecisionCtx c m => m [Decision]
restart = do
  traceInfo "restart" mempty
  uid   <- liftIO nextRandom
  task  <- view pActivityTask <$> view dcPlan
  event <- findEvent WorkflowExecutionStarted
  let input = join $ view weseaInput <$> join (view heWorkflowExecutionStartedEventAttributes <$> event)
  return [ scheduleActivity uid (task ^. tActivityType) (task ^. tTaskList) input ]

-- | Cancel failed workflow step.
--
canceled :: MonadDecisionCtx c m => m [Decision]
canceled = do
  traceInfo "canceled" mempty
  return [ cancelActivity ]

nothing :: MonadDecisionCtx c m => m [Decision]
nothing = do
  events <- view dcEvents
  traceError "none" [ "events" .= (show . view heEventType <$> events) ]
  return mempty

-- | Schedule decision based on history events.
--
schedule :: MonadDecisionCtx c m => m [Decision]
schedule = do
  traceInfo "schedule" mempty
  let f []                                                     = nothing
      f (e:es)
        | e ^. heEventType == WorkflowExecutionStarted         = begin e
        | e ^. heEventType == WorkflowExecutionCancelRequested = cancel
        | e ^. heEventType == ActivityTaskCompleted            = completed
        | e ^. heEventType == ActivityTaskCanceled             = completed
        | e ^. heEventType == ActivityTaskTimedOut             = restart
        | e ^. heEventType == ActivityTaskFailed               = restart
        | e ^. heEventType == RequestCancelActivityTaskFailed  = canceled
        | otherwise                                            = f es
  events <- view dcEvents
  f events

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
