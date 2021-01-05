{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Run (runApi) where

import           Chakra
import qualified Domain.API.User    as U
import           Import

type API = U.UserAPI :<|> EmptyAPI

appAPI:: Proxy API
appAPI = Proxy

appServer =  U.server :<|> emptyServer

appMiddlewares :: ToJSON a => a -> IO (Application -> Application)
appMiddlewares d = chakraMiddlewares d

runApi :: RIO AppConf ()
runApi = do
  hSetBuffering stdin LineBuffering
  appConfig <- ask
  userRepo <- liftIO $ U.newRepo
  middlewares <- liftIO $ appMiddlewares (appInfoDetail appConfig)
  runChakraAppWithMetrics
    middlewares
    (appLogFunc appConfig, (appInfoDetail appConfig))
    appAPI
    appServer
