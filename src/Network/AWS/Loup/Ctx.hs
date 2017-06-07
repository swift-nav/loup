{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contexts for monad transformers.
--
module Network.AWS.Loup.Ctx
  ( runAmazonCtx
  , runDecisionCtx
  ) where

import Control.Monad.Trans.AWS
import Network.AWS.Loup.Prelude
import Network.AWS.Loup.Types
import Network.AWS.SWF

-- | Catcher for exceptions, traces and rethrows.
--
botSomeExceptionCatch :: MonadCtx c m => SomeException -> m a
botSomeExceptionCatch ex = do
  traceError "exception" [ "error" .= displayException ex ]
  throwIO ex

-- | Catcher for exceptions, emits stats and rethrows.
--
topSomeExceptionCatch :: MonadStatsCtx c m => SomeException -> m a
topSomeExceptionCatch ex = do
  statsIncrement "exception" [ "reason" =. show ex ]
  throwIO ex

-- | Run bottom TransT.
--
runBotTransT :: (MonadMain m, HasCtx c) => c -> TransT c m a -> m a
runBotTransT c action = runTransT c $ catch action botSomeExceptionCatch

-- | Run top TransT.
--
runTopTransT :: (MonadMain m, HasStatsCtx c) => c -> TransT c m a -> m a
runTopTransT c action = runBotTransT c $ catch action topSomeExceptionCatch

-- | Run amazon context.
--
runAmazonCtx :: MonadStatsCtx c m => TransT AmazonCtx m a -> m a
runAmazonCtx action = do
  c <- view statsCtx
  e <- newEnv Oregon $ FromEnv "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" mempty
  runTopTransT (AmazonCtx c e) action

-- | Run decision context.
--
runDecisionCtx :: MonadAmazonCtx c m => Plan -> [HistoryEvent] -> TransT DecisionCtx m a -> m a
runDecisionCtx plan events action = do
  c <- view amazonCtx
  runBotTransT (DecisionCtx c plan events) action
