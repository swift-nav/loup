{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Actor logic.
--
module Network.AWS.Loup.Act
  ( activity
  , act
  , actMain
  ) where

import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.AWS
import Data.Yaml
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
    pure (pfatrs ^. pfatrsTaskToken, pfatrs ^. pfatrsInput)

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
    pure (rathrs ^. rathrsCancelRequested)

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
runActivity :: MonadStatsCtx c m => Bool -> Text -> Maybe Value -> m ()
runActivity copy command input = do
  traceInfo "run" [ "command" .= command, "input" .= input ]
  intempdir copy $ do
    liftIO $ maybe_ input $ encodeFile "input.json"
    stderr $ inshell command mempty

-- | Actor logic - poll for work, download artifacts, run command, upload artifacts.
--
activity :: (MonadStatsCtx c m, FromJSON a) => Text -> Text -> Int -> (Maybe a -> m b) -> m ()
activity domain queue interval action = do
  traceInfo "poll" mempty
  (token, input) <- pollActivity domain (taskList queue)
  maybe_ token $ \token' -> do
    traceInfo "start" mempty
    let input' = join $ decode . encodeUtf8 <$> input
    race_ (runHeartbeat token' interval) $ do
      void $ action input'
      failActivity token'
    traceInfo "finish" mempty

-- | Activity setup fom main.
--
act :: MonadStatsCtx c m => Text -> Text -> Int -> Bool -> Text -> m ()
act domain queue interval copy command =
  preStatsCtx [ "label" .= LabelAct, "domain" .= domain, "queue" .= queue ] $
    activity domain queue interval $
      runActivity copy command

-- | Run actor from main with configuration.
--
actMain :: MonadControl m => Text -> Text -> Int -> Int -> Bool -> Text -> m ()
actMain domain queue count interval copy command =
  runCtx $ runStatsCtx $
    runConcurrent $ replicate count $ forever $ act domain queue interval copy command
