{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Configuration.Dotenv       (defaultConfig, loadFile)
import qualified Data.Text                  as T
import           Import
import qualified Logging                    as L
import           Options.Applicative.Simple
import qualified Paths_ApiTemplate
import           RIO.Process
import           Run

main :: IO ()
main = do
  _ <- loadFile defaultConfig -- load .env file if available
  pc <- mkDefaultProcessContext
  (_, lf) <- L.newLogger (L.LogStderr L.defaultBufSize) L.jsonFormatter
  withAppSettingsFromEnv $ \appSettings ->
    let app =
          AppConf
            { appLogFunc = lf
            , appProcessContext = pc
            , appInfoDetail = appSettings {appVersion = T.pack ver}
            }
        ver = $(simpleVersion Paths_ApiTemplate.version) -- TH to get cabal project's git sha version
      in runRIO app runApi
