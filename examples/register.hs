#!/usr/bin/env stack
-- stack runghc --package turtle --package loup --package yaml

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml        hiding (Parser)
import Network.AWS.Loup
import Turtle

-- | aws command.
--
aws :: MonadIO m => Text -> [Text] -> m ()
aws cmd args = void $ procStrict "aws" (cmd : args) mempty

-- | swf command.
--
swf :: MonadIO m => Text -> [Text] -> m ()
swf cmd args = aws "swf" (cmd : args)

-- | Register decision task.
--
decision :: MonadIO m => Text -> Task -> m ()
decision domain task =
  swf "register-workflow-type"
    [ "--domain"                                   , domain
    , "--name"                                     , task ^. tActivityType ^. atName
    , "--workflow-version"                         , task ^. tActivityType ^. atVersion
    , "--default-execution-start-to-close-timeout" , "31536000"
    , "--default-task-start-to-close-timeout"      , "15"
    ]

-- | Register activity task.
--
activity :: MonadIO m => Text -> Task -> m ()
activity domain task =
  swf "register-activity-type"
    [ "--domain"                                 , domain
    , "--name"                                   , task ^. tActivityType ^. atName
    , "--activity-version"                       , task ^. tActivityType ^. atVersion
    , "--default-task-schedule-to-start-timeout" , "15"
    , "--default-task-start-to-close-timeout"    , "NONE"
    , "--default-task-heartbeat-timeout"         , "15"
    , "--default-task-schedule-to-close-timeout" , "NONE"
    ]

-- | Handle plan.
--
register :: MonadIO m => Text -> Plan -> m ()
register domain plan = do
  decision domain (plan ^. pDecisionTask)
  activity domain (plan ^. pActivityTask)

-- | Handle plans.
--
registers :: MonadIO m => Text -> [Plan] -> m ()
registers domain = mapM_ (register domain)

-- | Arguments to pass
--
parser :: Parser (Text, Text)
parser = (,) <$> optText "domain" 'd' "Domain" <*> optText "plan" 'p' "Plan File"

-- | main entry point.
--
main :: IO ()
main = do
  (domain, file) <- options "Register" parser
  plans          <- liftIO $ join . maybeToList <$> decodeFile (textToString file)
  registers domain plans
