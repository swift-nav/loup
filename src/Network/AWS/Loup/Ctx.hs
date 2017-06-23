{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contexts for monad transformers.
--
module Network.AWS.Loup.Ctx
  ( runAmazonCtx
  , preAmazonCtx
  , runDecisionCtx
  ) where

import Control.Exception.Lifted
import Control.Monad.Trans.AWS
import Data.Aeson
import Network.AWS.Loup.Prelude
import Network.AWS.Loup.Types
import Network.AWS.SWF

-- | Catcher for exceptions, traces and rethrows.
--
botSomeExceptionCatch :: MonadCtx c m => SomeException -> m a
botSomeExceptionCatch ex = do
  traceError "exception" [ "error" .= displayException ex ]
  throwIO ex

-- | Catch TransportError's.
--
botErrorCatch :: MonadCtx c m => Error -> m a
botErrorCatch ex = do
  case ex of
    TransportError _ ->
      return ()
    _ ->
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
runBotTransT :: (MonadControl m, HasCtx c) => c -> TransT c m a -> m a
runBotTransT c action = runTransT c $ catches action [ Handler botErrorCatch, Handler botSomeExceptionCatch ]

-- | Run top TransT.
--
runTopTransT :: (MonadControl m, HasStatsCtx c) => c -> TransT c m a -> m a
runTopTransT c action = runBotTransT c $ catch action topSomeExceptionCatch

-- | Run amazon context.
--
runAmazonCtx :: MonadStatsCtx c m => TransT AmazonCtx m a -> m a
runAmazonCtx action = do
  c <- view statsCtx
  e <- newEnv Oregon $ FromEnv "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY" mempty
  runTopTransT (AmazonCtx c e) action

-- | Update amazon context's preamble.
--
preAmazonCtx :: MonadAmazonCtx c m => Pairs -> TransT AmazonCtx m a -> m a
preAmazonCtx preamble action = do
  c <- view amazonCtx <&> cPreamble <>~ preamble
  runBotTransT c action

-- | Run decision context.
--
runDecisionCtx :: MonadAmazonCtx c m => Plan -> [HistoryEvent] -> TransT DecisionCtx m a -> m a
runDecisionCtx plan events action = do
  c <- view amazonCtx
  runBotTransT (DecisionCtx c plan events) action
