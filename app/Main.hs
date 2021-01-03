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
  let ver = $(simpleVersion Paths_ApiTemplate.version)
  _ <- loadFile defaultConfig
  (options, ()) <- optionsParser ver
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  infoDetail <- getAppEnv
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appInfoDetail = infoDetail
          }
     in runRIO app runApi

optionsParser :: Monoid b => String -> IO (Options, b)
optionsParser version = simpleOptions
  version
  "WebAPI" -- name of the domain app
  "API interface for the domain"
  (Options
      <$> switch ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
                )
  )
  empty

-- getAppEnv :: IO (Either String InfoDetail)
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
