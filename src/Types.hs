{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import qualified Data.Aeson  as A
import           Logging
import           RIO
import           RIO.Process
import           System.Envy

data AppConf = AppConf
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appInfoDetail     :: !InfoDetail
  }

instance HasLogFunc AppConf where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext AppConf where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

data InfoDetail = InfoDetail
  {
    appName        :: !Text
  , appVersion     :: !Text
  , appDescription :: !Text
  }
  deriving (Show,Eq,Generic)

instance FromEnv InfoDetail
instance A.ToJSON InfoDetail where
  toJSON =
    A.genericToJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 3}

type AppCtx = (ModLogger, InfoDetail)
type App = RIO AppCtx
