{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various product types.
--
module Network.AWS.Loup.Types.Product
  ( module Network.AWS.Loup.Types.Product
  ) where

import Data.Aeson.TH
import Network.AWS.Loup.Prelude

-- | Plan Task
--
-- Work task.
--
data Task = Task
  { _tName    :: Text
    -- ^ Name of task.
  , _tVersion :: Text
    -- ^ Version of task.
  , _tQueue   :: Text
    -- ^ Queue for task.
  } deriving (Show, Eq)

$(makeLenses ''Task)
$(deriveJSON spinalOptions ''Task)

-- | Plan
--
-- Group of tasks.
--
data Plan = Plan
  { _pDecision :: Task
    -- ^ Decision task.
  , _pActiviy  :: Task
    -- ^ Activity task.
  } deriving (Show, Eq)

$(makeLenses ''Plan)
$(deriveJSON spinalOptions ''Plan)
