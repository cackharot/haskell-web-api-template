{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import qualified Data.Aeson  as A
-- import qualified Data.Aeson.Parser
-- import           Data.Aeson.Types
import           RIO
import           RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })


data InfoDetail = InfoDetail
  {
    appName        :: Text
  , appVersion     :: Text
  , appDescription :: Text
  }
  deriving (Show,Eq,Generic)

instance A.ToJSON InfoDetail
