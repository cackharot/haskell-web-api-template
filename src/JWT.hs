{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module JWT
where

import           Control.Monad.Except
import           Crypto.JOSE
import qualified Data.Aeson           as Aeson
import           Data.ByteString      (readFile)
import           RIO
import           Servant.Auth.Server
import           System.Environment

acquireJwks :: IO JWKSet
acquireJwks = do
  envUrl <- lookupEnv "JWK_PATH"
  let jwkPath = fromMaybe "secrets/jwk.sig" envUrl
  fileContent <- readFile jwkPath
  let parsed = Aeson.eitherDecodeStrict fileContent
  return $ either (\e -> error $ "Invalid JWK file: " <> e) id parsed

getJWTSettings :: JWK -> JWKSet -> JWTSettings
getJWTSettings sigKey jwkSet =
  JWTSettings
    { signingKey = sigKey
    , jwtAlg = Nothing
    , validationKeys = vkeys sigKey jwkSet
    , audienceMatches = const Matches
    }
  where
    vkeys k (JWKSet x) = JWKSet (x ++ [k])
