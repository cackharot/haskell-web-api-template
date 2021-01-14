#!/usr/bin/env stack
{- stack --resolver lts-14.27 runghc
 --package ApiTemplate
-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators #-}
import Chakra

type HelloRoute = "hello" :> QueryParam "name" Text :> Get '[PlainText] Text
type API = HelloRoute :<|> EmptyAPI

hello :: Maybe Text -> BasicApp Text
hello name = do
  let name' = fromMaybe "Sensei!" name
  logInfo $ "Saying hello to " <> display name'
  return $ "Hello " <> name' <> "!"

main :: IO ()
main = do
  let infoDetail = InfoDetail "example" "dev" "0.1" "change me"
      appEnv = appEnvironment infoDetail
      appVer = appVersion infoDetail
      appAPI = Proxy :: Proxy API
      appServer = hello :<|> emptyServer
  logFunc <- buildLogger appEnv appVer
  middlewares <- chakraMiddlewares infoDetail
  runChakraAppWithMetrics
    middlewares
    EmptyContext
    (logFunc, infoDetail)
    appAPI
    appServer
