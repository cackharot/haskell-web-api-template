{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson             as A
import qualified Data.Text              as T
import           Domain.Types           (User (User))
import           Logging
import           RIO
import           RIO.Process
import           Servant.Auth.Server
import           System.Envy

data AppConf = AppConf
  { appLogFunc        :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appInfoDetail     :: !InfoDetail
  }

instance HasLogFunc AppConf where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext AppConf where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

data InfoDetail = InfoDetail
  { appName        :: !Text,
    appEnvironment :: !Text,
    appVersion     :: !Text,
    appDescription :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromEnv InfoDetail

instance A.ToJSON InfoDetail where
  toJSON =
    A.genericToJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 3}

data AuthenticatedUser = AuthenticatedUser
  { aud   :: !Text,
    iss   :: !Text,
    appid :: !Text,
    aio   :: !Text,
    oid   :: !Text,
    sub   :: !Text,
    tid   :: !Text
  }
  deriving (Show, Generic)

instance A.ToJSON AuthenticatedUser

instance A.FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser where
  decodeJWT m = case A.fromJSON . A.toJSON $ m of
    A.Error e   -> Left $ T.pack e
    A.Success a -> Right a

data UserRepo = UserRepo
  { _getUser     :: !(Text -> IO (Maybe User)),
    _getAllUsers :: !(IO [User]),
    _insertUser  :: !(Text -> User -> IO ())
  }

class HasUserRepo env where
  userRepoL :: Lens' env UserRepo

instance {-# OVERLAPPABLE #-} Has UserRepo m => HasUserRepo m where
  userRepoL = hasLens

type AppCtx = (ModLogger, InfoDetail, UserRepo)

type App = RIO AppCtx
