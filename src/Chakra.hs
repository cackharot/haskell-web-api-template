{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Trustworthy #-}

{-| 
This module re-exports all functionality of this package for easy use.
Users expected to import this module only.

__Examples:__

@
__import__ "Chakra"
@

== Getting started

To create a bare minimum API service all you need is below:

@
\#!\/usr\/bin\/env stack
\{\- stack --resolver lts-14.27 runghc --package chakra \-\}
\{\-\# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators \#\-\}
import RIO
import Chakra
import Servant

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
@
-}
module Chakra
  (
    -- $main
    module Chakra.App,
    -- $config
    module Chakra.Config,
    -- $jwt
    module Chakra.JWT,
    -- $logging
    module Chakra.Logging,
    -- $types
    module Chakra.Types,
    -- $util
    module Chakra.Util,
    module Chakra,
  )
where

import Chakra.App
import Chakra.Config
import Chakra.JWT
import Chakra.Logging
import Chakra.Types
import Chakra.Util
import RIO

-- | Basic application context, mostly used in examples.
-- For real life you need to create one for your application
type BasicAppCtx = (ModLogger, InfoDetail)

-- | Nice type synonym to mark your servant handlers
-- For real life you need to create one for your application
type BasicApp = RIO BasicAppCtx


{- $codec
 Main App functionalities
-}

{- $config
 Configuration reading from ENV utility functions
-}

{- $logging
 Logging related functionalities
-}

{- $jwt
 JWT based authentication functionalities
-}

{- $types
 Package defined Types
-}

{- $util
 Assorted conventient functions
-}
