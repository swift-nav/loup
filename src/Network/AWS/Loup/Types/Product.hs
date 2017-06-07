{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various product types.
--
module Network.AWS.Loup.Types.Product
  ( module Network.AWS.Loup.Types.Product
  ) where

import Data.Aeson.TH
import Network.AWS.Loup.Prelude
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
  , _pActiviyTask  :: Task
    -- ^ Activity task.
  } deriving (Show, Eq)

$(makeLenses ''Plan)
$(deriveJSON spinalOptions ''Plan)
