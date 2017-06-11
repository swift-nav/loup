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
import Turtle                          hiding (input)

-- | Poll for activity.
--
pollActivity :: MonadAmazonCtx c m => Text -> TaskList -> m (Maybe Text, Maybe Text)
pollActivity domain list = do
  pfatrs <- send $ pollForActivityTask domain list
  return (pfatrs ^. pfatrsTaskToken, pfatrs ^. pfatrsInput)

-- | Cancel activity.
--
cancelActivity :: MonadAmazonCtx c m => Text -> m ()
cancelActivity token =
  void $ send $ respondActivityTaskCanceled token

-- | Fail activity.
--
failActivity :: MonadAmazonCtx c m => Text -> m ()
failActivity token =
  void $ send $ respondActivityTaskFailed token

-- | Hearbeat.
--
heartbeat :: MonadAmazonCtx c m => Text -> m Bool
heartbeat token = do
  rathrs <- send $ recordActivityTaskHeartbeat token
  return $ rathrs ^. rathrsCancelRequested

-- | Run a managed action inside a temp directory.
--
intempdir :: MonadControl m => Managed a -> m ()
intempdir action =
  bracket pwd cd $ \fromdir ->
    sh $ using $ do
      todir <- mktempdir "/tmp" "loup-"
      cptree fromdir todir
      cd todir
      action

-- | Run heartbeat.
--
runHeartbeat :: MonadAmazonCtx c m => Text -> Int -> m ()
runHeartbeat token interval = do
  traceInfo "heartbeat" mempty
  liftIO $ threadDelay $ interval * 1000000
  nok <- heartbeat token
  if not nok then runHeartbeat token interval else do
    traceInfo "cancel" mempty
    cancelActivity token

-- | Run command with input.
--
runActivity :: MonadAmazonCtx c m => Text -> Text -> Maybe Text -> m ()
runActivity token command input = do
  traceInfo "run" [ "command" .= command, "input" .= input]
  intempdir $ do
    liftIO $ maybe_ input $ writeTextFile "input.json"
    stdout $ inshell command mempty
  failActivity token

-- | Actor logic - poll for work, download artifacts, run command, upload artifacts.
--
act :: MonadAmazonCtx c m => Text -> Text -> Int -> Text -> m ()
act domain queue interval command =
  preAmazonCtx [ "label" .= LabelAct, "domain" .= domain, "queue" .= queue ] $ do
    traceInfo "poll" mempty
    (token, input) <- pollActivity domain (taskList queue)
    maybe_ token $ \token' -> do
      traceInfo "start" mempty
      race_ (runHeartbeat token' interval) (runActivity token' command input)
      traceInfo "finish" mempty

-- | Run actor from main with configuration.
--
actMain :: MonadControl m => Text -> Text -> Int -> Text -> m ()
actMain domain queue interval command =
  runResourceT $ runCtx $ runStatsCtx $ runAmazonCtx $
    forever $ act domain queue interval command
