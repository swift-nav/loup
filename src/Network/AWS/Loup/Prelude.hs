{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Local Prelude.
--
module Network.AWS.Loup.Prelude
  ( module Exports
  , encode'
  , pages
  , runConcurrent
  ) where

import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.AWS
import Data.Aeson
import Data.ByteString.Lazy
import Data.Conduit
import Data.Conduit.List
import Preamble                        as Exports

-- | Encode JSON to Text.
--
encode' :: ToJSON a => a -> Text
encode' = decodeUtf8 . toStrict . encode

-- | Paginate all.
--
pages :: (AWSConstraint c m, AWSPager a) => a -> m [Rs a]
pages a = paginate a $$ consume

-- | Run a list of actions concurrently.
--
runConcurrent :: MonadBaseControl IO m => [m a] -> m ()
runConcurrent = void . runConcurrently . traverse Concurrently

