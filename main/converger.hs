{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run converger.
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
  , pool   :: FilePath
    -- ^ Pool file to converge on.
  } deriving (Show, Generic)

instance ParseRecord Args

-- | Run converger.
--
main :: IO ()
main = do
  args <- getRecord "Converger"
  convergeMain
    (domain args)
    (pool args)
