{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run actor.
--
import Network.AWS.Loup
import Options.Generic

-- | Args
--
-- Program arguments.
--
data Args = Args
  { domain   :: Text
    -- ^ Workflow domain.
  , queue    :: Text
    -- ^ Queue to listen to act on.
  , interval :: Int
    -- ^ Interval to heartbeat at.
  , command  :: Text
    -- ^ Command to run.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run actor.
--
main :: IO ()
main = do
  args <- getRecord "Actor"
  actMain
    (domain args)
    (queue args)
    (interval args)
    (command args)