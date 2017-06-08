{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various sum types.
--
module Network.AWS.Loup.Types.Sum where

import Data.Aeson.TH
import Network.AWS.Loup.Prelude

-- | LabelType
--
-- Tags for referencing workers.
--
data LabelType
  = LabelAct
  | LabelDecide
  | LabelConverge
  deriving (Show, Eq)

$(deriveJSON spinalOptions ''LabelType)
