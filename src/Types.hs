{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import qualified Data.Aeson  as A
import           RIO
import           RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , port           :: !Int
  }

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  , appInfoDetail     :: !InfoDetail
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

data InfoDetail = InfoDetail
  {
    appName        :: !Text
  , appVersion     :: !Text
  , appDescription :: !Text
  }
  deriving (Show,Eq,Generic)

instance A.ToJSON InfoDetail where
  toJSON =
    A.genericToJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 3}
