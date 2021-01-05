{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Run (runApi) where

import           Chakra
import qualified Domain.API.User    as U
import           Import

import           Network.Wai.Cli
import           System.Environment (lookupEnv)

type API = U.UserAPI :<|> EmptyAPI

appAPI:: Proxy API
appAPI = Proxy

appServer =  U.server :<|> emptyServer

appMiddlewares :: ToJSON a => Bool -> a -> Application -> Application
appMiddlewares e d = chakraMiddlewares e d

runApi :: RIO AppConf ()
runApi = do
  hSetBuffering stdin LineBuffering
  appConfig <- ask
  userRepo <- liftIO $ U.newRepo
  _ <- registerMetrics
  isProdEnv <-
    liftIO $ do maybe False (== "PRODUCTION") <$> lookupEnv "APP_ENVIRONMENT"
  let app' = appMiddlewares isProdEnv appInfoD
      appInfoD = (appInfoDetail appConfig)
      ctx  = (appLogFunc appConfig, appInfoD)
    in liftIO $ defWaiMain $ app' $ chakraApp appAPI EmptyContext ctx $ appServer
