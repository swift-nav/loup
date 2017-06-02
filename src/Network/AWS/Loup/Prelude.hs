{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Local Prelude.
--
module Network.AWS.Loup.Prelude
  ( module Exports
  , runConcurrent
  ) where

import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Control
import Preamble                        as Exports

-- | Run a list of actions concurrently.
--
runConcurrent :: MonadBaseControl IO m => [m a] -> m ()
runConcurrent = void . runConcurrently . sequenceA . map Concurrently
