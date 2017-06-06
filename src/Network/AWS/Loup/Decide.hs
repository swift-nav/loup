{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Decider logic.
--
module Network.AWS.Loup.Decide
  ( decide
  , decideMain
  ) where

import Data.Yaml
import Network.AWS.Loup.Ctx
import Network.AWS.Loup.Prelude
import Network.AWS.Loup.Types

-- | Decider logic - poll for decisions, make decisions.
--
decide :: MonadAmazonCtx c m => Text -> [Plan] -> m ()
decide _domain _plans = undefined

-- | Run decider from main with configuration.
--
decideMain :: MonadControl m => Text -> FilePath -> m ()
decideMain domain pf =
  runResourceT $ runCtx $ runStatsCtx $ runAmazonCtx $ do
    plans <- liftIO $ join . maybeToList <$> decodeFile pf
    decide domain plans
