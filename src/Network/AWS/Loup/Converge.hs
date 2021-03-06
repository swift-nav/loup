{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Converger logic.
--
module Network.AWS.Loup.Converge
  ( converging
  , converge
  , convergeMain
  ) where

import Control.Monad.Trans.AWS
import Data.HashSet             hiding (filter)
import Data.Time.Clock.POSIX
import Data.Yaml
import Network.AWS.Loup.Ctx
import Network.AWS.Loup.Prelude hiding (delete)
import Network.AWS.Loup.Types
import Network.AWS.SWF

-- | List open workflows.
--
listWorkflows :: MonadStatsCtx c m => Text -> ActivityType -> m [Text]
listWorkflows domain activity =
  runResourceT $ runAmazonCtx $ do
    let etf = executionTimeFilter $ posixSecondsToUTCTime $ fromIntegral (0 :: Int)
        wtf = workflowTypeFilter (activity ^. atName)
    weis <- pages $ set loweTypeFilter (pure wtf) $ listOpenWorkflowExecutions domain etf
    let predicate wei = maybe True not $ wei ^. weiCancelRequested
    pure $ view weWorkflowId . view weiExecution <$> filter predicate (join $ view weiExecutionInfos <$> weis)

-- | Start a workflow.
--
startWorkflow :: MonadStatsCtx c m => Text -> ActivityType -> TaskList -> Text -> Maybe Text -> m ()
startWorkflow domain activity list wid input =
  runResourceT $ runAmazonCtx $ do
    let wt = workflowType (activity ^. atName) (activity ^. atVersion)
    void $ send $ startWorkflowExecution domain wid wt
      & sTaskList .~ pure list
      & sInput .~ input

-- | Cancel a workflow.
--
cancelWorkflow :: MonadStatsCtx c m => Text -> Text -> m ()
cancelWorkflow domain wid =
  runResourceT $ runAmazonCtx $
    void $ send $ requestCancelWorkflowExecution domain wid

-- | Converger logic - get running workers and converge against pool.
--
converging :: MonadStatsCtx c m => Text -> Pool -> m ()
converging domain pool =
  preStatsCtx [ "label" .= LabelDecide, "domain" .= domain ] $ do
    let activity = pool ^. pTask ^. tActivityType
    wids <- fromList <$> listWorkflows domain activity
    let fold kvs as action = do
          let g k v bs = do
                let k' = k -.- textShow (hash v)
                if k' `member` bs then pure $ k' `delete` bs else action k' v >> pure bs
          ifoldrM g as kvs
    wids' <- fold (pool ^. pWorkers) wids $ \wid input -> do
      traceInfo "start" [ "wid" .= wid, "input" .= input ]
      startWorkflow domain activity (pool ^. pTask ^. tTaskList) wid (encode' <$> input)
    forM_ wids' $ \wid -> do
      traceInfo "cancel" [ "wid" .= wid ]
      cancelWorkflow domain wid

-- | Converging setup from main.
--
converge :: MonadStatsCtx c m => Text -> Pool -> m ()
converge domain pool =
  preStatsCtx [ "label" .= LabelDecide, "domain" .= domain ] $
    converging domain pool

-- | Run converger from main with configuration.
--
convergeMain :: MonadControl m => Text -> FilePath -> m ()
convergeMain domain file =
  runCtx $ runStatsCtx $ do
    pools <- liftIO $ join . maybeToList <$> decodeFile file
    runConcurrent $ converge domain <$> pools
