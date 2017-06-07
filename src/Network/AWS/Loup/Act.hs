{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Actor logic.
--
module Network.AWS.Loup.Act
  ( act
  , actMain
  ) where

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

-- | Successful activity completion.
--
completeActivity :: MonadAmazonCtx c m => Text -> m ()
completeActivity token =
  void $ send $ respondActivityTaskCompleted token

-- | Hearbeat.
--
heartbeat :: MonadAmazonCtx c m => Text -> m ()
heartbeat token =
  void $ send $ recordActivityTaskHeartbeat token

-- | Run a managed action inside a temp directory.
--
intempdir :: MonadIO m => Managed a -> m ()
intempdir action =
  sh $ using $ do
    fromdir <- pwd
    todir   <- mktempdir "/tmp" "loup-"
    cptree fromdir todir
    cd todir
    action

-- | Run command with input.
--
runActivity :: MonadCtx c m => Text -> Maybe Text -> m ()
runActivity command input = do
  traceInfo "run" [ "command" .= command, "input" .= input]
  intempdir $ do
    liftIO $ maybe_ input $ writeTextFile "input.json"
    stdout $ flip inshell mempty $ "." -/- command

-- | Actor logic - poll for work, download artifacts, run command, upload artifacts.
--
act :: MonadAmazonCtx c m => Text -> Text -> Int -> Text -> m ()
act domain queue interval command =
  preAmazonCtx [ "label" .= LabelAct, "domain" .= domain, "queue" .= queue ] $ do
    traceInfo "poll" mempty
    (token, input) <- pollActivity domain (taskList queue)
    maybe_ token $ \token' -> do
      traceInfo "start" mempty
      race_ (runEvery (interval * 1000000) $ heartbeat token') (runActivity command input)
      completeActivity token'
      traceInfo "finish" mempty

-- | Run actor from main with configuration.
--
actMain :: MonadControl m => Text -> Text -> Int -> Text -> m ()
actMain domain queue interval command =
  runResourceT $ runCtx $ runStatsCtx $ runAmazonCtx $
    forever $ act domain queue interval command
