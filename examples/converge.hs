#!/usr/bin/env stack
-- stack runghc --package turtle --package loup --package yaml --package aeson

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Data.Aeson.TH
import Data.Yaml        hiding (Parser)
import Network.AWS.Loup
import Turtle

data Worker = Worker
  { _wInput :: Value
  } deriving (Show, Eq)

$(makeLenses ''Worker)
$(deriveJSON spinalOptions ''Worker)

data Workers = Workers
  { _wActivityType :: ActivityType
  , _wWorkerMap    :: HashMap Text Worker
  } deriving (Show, Eq)

$(makeLenses ''Workers)
$(deriveJSON spinalOptions ''Workers)

-- | aws command.
--
aws :: MonadIO m => Text -> [Text] -> m ()
aws cmd args = void $ procStrict "aws" (cmd : args) mempty

-- | swf command.
--
swf :: MonadIO m => Text -> [Text] -> m ()
swf cmd args = aws "swf" (cmd : args)

-- | Arguments to pass
--
parser :: Parser (Text, Text, Text)
parser = (,,) <$> optText "domain" 'd' "Domain" <*> optText "plan" 'p' "Plan File" <*> optText "workers" 'w' "Workers File"

-- | main entry point.
--
main :: IO ()
main = do
  (domain, pf, wf) <- options "Converge" parser
  plans            <- liftIO $ join . maybeToList <$> decodeFile (textToString pf)
  workers          <- liftIO $ join . maybeToList <$> decodeFile (textToString wf)
  print (plans :: [Plan])
  print (workers :: [Workers])

