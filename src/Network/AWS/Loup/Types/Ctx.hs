{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Context objects for monad transformers.
--
module Network.AWS.Loup.Types.Ctx
  ( module Network.AWS.Loup.Types.Ctx
  ) where

import Control.Monad.Trans.AWS
import Network.AWS.Loup.Prelude

-- | AmazonCtx
--
-- Amazon context.
--
data AmazonCtx = AmazonCtx
  { _acStatsCtx :: StatsCtx
    -- ^ Parent context.
  , _acEnv      :: Env
    -- ^ Amazon environment.
  }

$(makeClassyConstraints ''AmazonCtx [''HasStatsCtx, ''HasEnv])

instance HasStatsCtx AmazonCtx where
  statsCtx = acStatsCtx

instance HasCtx AmazonCtx where
  ctx = statsCtx . ctx

instance HasEnv AmazonCtx where
  environment = acEnv

type MonadAmazonCtx c m =
  ( MonadStatsCtx c m
  , HasAmazonCtx c
  , AWSConstraint c m
  )
