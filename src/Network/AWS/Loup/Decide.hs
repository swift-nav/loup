{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Decider logic.
--
module Network.AWS.Loup.Decide
  ( deciding
  , decide
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
pollDecision :: MonadStatsCtx c m => Text -> TaskList -> m (Maybe Text, [HistoryEvent])
pollDecision domain list =
  runResourceT $ runAmazonCtx $ do
    pfdtrs <- pages $ pollForDecisionTask domain list
    pure (join $ headMay $ view pfdtrsTaskToken <$> pfdtrs, reverse $ join $ view pfdtrsEvents <$> pfdtrs)

-- | Successful decision completion.
--
completeDecision :: MonadStatsCtx c m => Text -> [Decision] -> m ()
completeDecision token decisions =
  runResourceT $ runAmazonCtx $
    void $ send $ respondDecisionTaskCompleted token
      & rdtcDecisions .~ decisions

-- | Schedule activity decision.
--
scheduleActivity :: UUID -> ActivityType -> TaskList -> Maybe Text -> Decision
scheduleActivity uid activity list input = do
  let satda = scheduleActivityTaskDecisionAttributes activity (toText uid)
        & satdaTaskList .~ pure list
        & satdaInput    .~ input
  decision ScheduleActivityTask
    & dScheduleActivityTaskDecisionAttributes .~ pure satda

-- | Complete activity decision.
--
completeActivity :: Decision
completeActivity = do
  let cweda = completeWorkflowExecutionDecisionAttributes
  decision CompleteWorkflowExecution
    & dCompleteWorkflowExecutionDecisionAttributes .~ pure cweda

-- | Cancel activity decision.
--
cancelActivity :: Decision
cancelActivity = do
  let cweda = cancelWorkflowExecutionDecisionAttributes
  decision CancelWorkflowExecution
    & dCancelWorkflowExecutionDecisionAttributes .~ pure cweda

-- | Request to cancel activity decision.
--
requestCancel :: UUID -> Decision
requestCancel uid = do
  let rcatda = requestCancelActivityTaskDecisionAttributes (toText uid)
  decision RequestCancelActivityTask
    & dRequestCancelActivityTaskDecisionAttributes .~ pure rcatda

-- | Find a matching event type.
--
findEvent :: MonadDecisionCtx c m => EventType -> m (Maybe HistoryEvent)
findEvent eventType = do
  let f []     = pure Nothing
      f (e:es) = bool (f es) (pure $ Just e) $ e ^. heEventType == eventType
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
  pure [ scheduleActivity uid (task ^. tActivityType) (task ^. tTaskList) input ]

-- | Cancel workflow step.
--
cancel :: MonadDecisionCtx c m => m [Decision]
cancel = do
  traceInfo "cancel" mempty
  event <- findEvent ActivityTaskScheduled
  let uid = join $ fromText . view atseaActivityId <$> join (view heActivityTaskScheduledEventAttributes <$> event)
  pure [ maybe cancelActivity requestCancel uid ]

-- | Completed workflow step.
--
completed :: MonadDecisionCtx c m => m [Decision]
completed = do
  traceInfo "completed" mempty
  pure [ completeActivity ]

-- | Restart workflow step.
--
restart :: MonadDecisionCtx c m => m [Decision]
restart = do
  traceInfo "restart" mempty
  uid   <- liftIO nextRandom
  task  <- view pActivityTask <$> view dcPlan
  event <- findEvent WorkflowExecutionStarted
  let input = join $ view weseaInput <$> join (view heWorkflowExecutionStartedEventAttributes <$> event)
  pure [ scheduleActivity uid (task ^. tActivityType) (task ^. tTaskList) input ]

-- | Cancel failed workflow step.
--
canceled :: MonadDecisionCtx c m => m [Decision]
canceled = do
  traceInfo "canceled" mempty
  pure [ cancelActivity ]

-- | When we run out of events.
--
nothing :: MonadDecisionCtx c m => m [Decision]
nothing = do
  events <- view dcEvents
  traceError "none" [ "events" .= (textShow . view heEventType <$> events) ]
  pure mempty

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
deciding :: MonadStatsCtx c m => Text -> Plan -> m ()
deciding domain plan = do
  traceInfo "poll" mempty
  (token, events) <- pollDecision domain (plan ^. pDecisionTask ^. tTaskList)
  maybe_ token $ \token' -> do
    traceInfo "start" mempty
    runDecisionCtx plan events $ do
      decisions <- schedule
      completeDecision token' decisions
    traceInfo "finish" mempty

-- | Deciding setup from main.
--
decide :: MonadStatsCtx c m => Text -> Plan -> m ()
decide domain plan =
  preStatsCtx [ "label" .= LabelDecide, "domain" .= domain ] $
    deciding domain plan

-- | Run decider from main with configuration.
--
decideMain :: MonadControl m => Text -> FilePath -> m ()
decideMain domain file =
  runCtx $ runStatsCtx $ do
    plans <- liftIO $ join . maybeToList <$> decodeFile file
    runConcurrent $ forever . decide domain <$> plans
