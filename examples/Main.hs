{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Chakra
import Configuration.Dotenv (defaultConfig, loadFile)
import qualified Data.Text as T
import Options.Applicative.Simple
import qualified Paths_ApiTemplate
import Servant.Auth.Server as SAS
import qualified User as U

type API auths = (SAS.Auth auths AuthenticatedUser :> U.UserAPI) :<|> EmptyAPI

appAPI :: Proxy (API '[SAS.JWT])
appAPI = Proxy

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  _ <- loadFile defaultConfig -- load .env file if available
  -- Load the AppSettings data from ENV variables
  withAppSettingsFromEnv $ \appSettings -> do
    -- Override the version from cabal file
    let ver = $(simpleVersion Paths_ApiTemplate.version) -- TH to get cabal project's git sha version
        infoDetail = appSettings {appVersion = T.pack ver}
    logFunc <- buildLogger (appEnvironment infoDetail) (appVersion infoDetail)
    userRepo <- U.newInMemRepo
    middlewares <- chakraMiddlewares infoDetail
    jwtCfg <- getJWTAuthSettings
    let cookieCfg = defaultCookieSettings {cookieIsSecure = SAS.NotSecure}
        sctx = cookieCfg :. jwtCfg :. chakraErrorFormatters :. EmptyContext
        appServer = U.server :<|> emptyServer
    -- Run API server with JWT auth and in-mem user repo
    runChakraAppWithMetrics
      middlewares
      sctx
      (logFunc, infoDetail, userRepo)
      appAPI
      appServer
