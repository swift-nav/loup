{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Decider logic.
--
module Network.AWS.Loup.Decide
  ( decide
  , decideMain
  ) where

import Network.AWS.Loup.Prelude

-- | Decider logic - poll for decisions, make decisions.
--
decide :: MonadControl m => Text -> FilePath -> m ()
decide = undefined

-- | Run decider from main with configuration.
--
decideMain :: MonadControl m => Text -> FilePath -> m ()
decideMain = undefined
