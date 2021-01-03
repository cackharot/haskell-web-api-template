{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Configuration.Dotenv       (defaultConfig, loadFile)
import           Import
import           Options.Applicative.Simple
import qualified Paths_ApiTemplate
import           RIO.Process
import           Run
import           System.Envy

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  infoDetail <- getAppEnv
  lo <- logOptionsHandle stderr False
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appInfoDetail = infoDetail
          }
     in runRIO app runApi

getAppEnv :: IO InfoDetail
getAppEnv = do
  c <- runEnv $ gFromEnvCustom opt (Just d)
  return $ either (\_ -> d) id c
  where
    opt = defOption
    d =
      InfoDetail
        "WebAPI Template"
        (fromString ver)
        "WebAPI tempalte with batteries included!"
    ver = $(simpleVersion Paths_ApiTemplate.version)
