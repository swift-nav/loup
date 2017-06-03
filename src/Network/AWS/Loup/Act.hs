{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SWF Actor logic.
--
module Network.AWS.Loup.Act
  ( act
  , actMain
  ) where

import Network.AWS.Loup.Prelude

-- | Actor logic - poll for work, download artifacts, run command, upload artifacts.
--
act :: MonadStatsCtx c m => Text -> Text -> String -> m ()
act _domain _queue _command = undefined

-- | Run actor from main with configuration.
--
actMain :: MonadControl m => Text -> Text -> String -> m ()
actMain domain queue command =
  runResourceT $ runCtx $ runStatsCtx $
    forever $ act domain queue command
