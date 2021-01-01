{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Import
import           Options.Applicative.Simple
import qualified Paths_ApiTemplate
import           RIO.Process
import           Run

main :: IO ()
main = do
  let ver = $(simpleVersion Paths_ApiTemplate.version)
  (options, ()) <- optionsParser ver
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appInfoDetail = InfoDetail "WebAPI Template" (fromString ver) "WebAPI tempalte with batteries included!"
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
    <*> option auto (long "port"
                    <> short 'p'
                    <> help "Application port to bind to"
                    <> showDefault
                    <> value 3000
                    <> metavar "INT")
  )
  empty
