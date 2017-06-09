{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various product types.
--
module Network.AWS.Loup.Types.Product
  ( module Network.AWS.Loup.Types.Product
  , ActivityType
  , atName
  , atVersion
  , TaskList
  , tlName
  ) where

import Data.Aeson.TH
import Network.AWS.Loup.Prelude
import Network.AWS.Loup.Types.Alias
import Network.AWS.SWF

-- | Task
--
-- Activity and Decision task.
--
data Task = Task
  { _tActivityType :: ActivityType
    -- ^ Activity type.
  , _tTaskList     :: TaskList
    -- ^ Task list.
  } deriving (Show, Eq)

$(makeLenses ''Task)
$(deriveJSON spinalOptions ''Task)

-- | Plan
--
-- Group of tasks.
--
data Plan = Plan
  { _pDecisionTask :: Task
    -- ^ Decision task.
  , _pActivityTask :: Task
    -- ^ Activity task.
  } deriving (Show, Eq)

$(makeLenses ''Plan)
$(deriveJSON spinalOptions ''Plan)

-- | Pool
--
-- Pool of workers associated with a task.
--
data Pool = Pool
  { _pTask    :: Task
    -- ^ Workers task.
  , _pWorkers :: InputMap
    -- ^ Workers input map.
  } deriving (Show, Eq)

$(makeLenses ''Pool)
$(deriveJSON spinalOptions ''Pool)
