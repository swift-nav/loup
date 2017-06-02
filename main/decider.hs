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
  { domain :: Text
    -- ^ Configuration file.
  , plan   :: FilePath
    -- ^ Plan file to decide on.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run decider.
--
main :: IO ()
main = do
  args <- getRecord "Decider"
  decideMain
    (domain args)
    (plan args)
