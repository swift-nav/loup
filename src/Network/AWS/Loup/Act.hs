{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Actor logic.
--
module Network.AWS.Loup.Act
  ( act
  , actMain
  ) where

import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.AWS
import Network.AWS.Loup.Ctx
import Network.AWS.Loup.Prelude
import Network.AWS.Loup.Types
import Network.AWS.SWF
import Turtle                          hiding (count, input)

-- | Poll for activity.
--
pollActivity :: MonadStatsCtx c m => Text -> TaskList -> m (Maybe Text, Maybe Text)
pollActivity domain list =
  runResourceT $ runAmazonCtx $ do
    pfatrs <- send $ pollForActivityTask domain list
    return (pfatrs ^. pfatrsTaskToken, pfatrs ^. pfatrsInput)

-- | Cancel activity.
--
cancelActivity :: MonadStatsCtx c m => Text -> m ()
cancelActivity token =
  runResourceT $ runAmazonCtx $
    void $ send $ respondActivityTaskCanceled token

-- | Fail activity.
--
failActivity :: MonadStatsCtx c m => Text -> m ()
failActivity token =
  runResourceT $ runAmazonCtx $
    void $ send $ respondActivityTaskFailed token

-- | Hearbeat.
--
heartbeat :: MonadStatsCtx c m => Text -> m Bool
heartbeat token =
  runResourceT $ runAmazonCtx $ do
    rathrs <- send $ recordActivityTaskHeartbeat token
    return $ rathrs ^. rathrsCancelRequested

-- | Run a managed action inside a temp directory.
--
intempdir :: MonadControl m => Bool -> Managed a -> m ()
intempdir copy action =
  bracket pwd cd $ \fromdir ->
    sh $ using $ do
      todir <- mktempdir "/tmp" "loup-"
      when copy $
        cptree fromdir todir
      cd todir
      action

-- | Run heartbeat.
--
runHeartbeat :: MonadStatsCtx c m => Text -> Int -> m ()
runHeartbeat token interval = do
  traceInfo "heartbeat" mempty
  liftIO $ threadDelay $ interval * 1000000
  nok <- heartbeat token
  if not nok then runHeartbeat token interval else do
    traceInfo "cancel" mempty
    cancelActivity token

-- | Run command with input.
--
runActivity :: MonadStatsCtx c m => Text -> Bool -> Text -> Maybe Text -> m ()
runActivity token copy command input = do
  traceInfo "run" [ "command" .= command, "input" .= input]
  intempdir copy $ do
    liftIO $ maybe_ input $ writeTextFile "input.json"
    stderr $ inshell command mempty
  failActivity token

-- | Actor logic - poll for work, download artifacts, run command, upload artifacts.
--
act :: MonadStatsCtx c m => Text -> Text -> Int -> Bool -> Text -> m ()
act domain queue interval copy command =
  preStatsCtx [ "label" .= LabelAct, "domain" .= domain, "queue" .= queue ] $ do
    traceInfo "poll" mempty
    (token, input) <- pollActivity domain (taskList queue)
    maybe_ token $ \token' -> do
      traceInfo "start" mempty
      race_ (runHeartbeat token' interval) (runActivity token' copy command input)
      traceInfo "finish" mempty

-- | Run actor from main with configuration.
--
actMain :: MonadControl m => Text -> Text -> Int -> Int -> Bool -> Text -> m ()
actMain domain queue count interval copy command =
  runResourceT $ runCtx $ runStatsCtx $
    runConcurrent $ replicate count $ forever $ act domain queue interval copy command
