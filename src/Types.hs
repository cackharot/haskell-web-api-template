{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import qualified Data.Aeson          as A
import qualified Data.Text           as T
import           Logging
import           RIO
import           RIO.Process
import           Servant.Auth.Server
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
  , appEnvironment :: !Text
  , appVersion     :: !Text
  , appDescription :: !Text
  }
  deriving (Show,Eq,Generic)

instance FromEnv InfoDetail
instance A.ToJSON InfoDetail where
  toJSON =
    A.genericToJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 3}

data AuthenticatedUser =
  AuthenticatedUser
    { aud   :: !Text
    , iss   :: !Text
    , appid :: !Text
    , aio   :: !Text
    , oid   :: !Text
    , sub   :: !Text
    , tid   :: !Text
    }
  deriving (Show, Generic)

instance A.ToJSON AuthenticatedUser
instance A.FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser where
  decodeJWT m = case A.fromJSON . A.toJSON $ m of
                  A.Error e   -> Left $ T.pack e
                  A.Success a -> Right a

type AppCtx = (ModLogger, InfoDetail)
type App = RIO AppCtx
