{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Configuration.Dotenv       (defaultConfig, loadFile)
import qualified Data.Text                  as T
import           Import
import           Options.Applicative.Simple
import qualified Paths_ApiTemplate
import           RIO.Process
import           Run

main :: IO ()
main = do
  _ <- loadFile defaultConfig -- load .env file if available
  lo <- logOptionsHandle stderr False
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    withAppSettingsFromEnv $ \appSettings ->
      let app =
            AppConf
              { appLogFunc = lf
              , appProcessContext = pc
              , appInfoDetail = appSettings {appVersion = T.pack ver}
              }
          ver = $(simpleVersion Paths_ApiTemplate.version)
       in runRIO app runApi
