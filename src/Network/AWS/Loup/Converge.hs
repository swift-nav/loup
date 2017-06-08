{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Converger logic.
--
module Network.AWS.Loup.Converge
  ( converge
  , convergeMain
  ) where

import Data.Yaml
import Network.AWS.Loup.Ctx
import Network.AWS.Loup.Prelude
import Network.AWS.Loup.Types

-- | Converger logic - get running workers and converge against pool.
--
converge :: MonadAmazonCtx c m => Text -> Pool -> m ()
converge domain pool =
  preAmazonCtx [ "label" .= LabelDecide, "domain" .= domain, "pool" .= pool ] $ do
    undefined

-- | Run converger from main with configuration.
--
convergeMain :: MonadControl m => Text -> FilePath -> m ()
convergeMain domain file =
  runResourceT $ runCtx $ runStatsCtx $ runAmazonCtx $ do
    pools <- liftIO $ join . maybeToList <$> decodeFile file
    runConcurrent $ forever . converge domain <$> pools
