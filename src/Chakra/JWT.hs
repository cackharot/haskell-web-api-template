{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chakra.JWT where

import           Control.Lens         (preview)
import           Control.Monad.Except (Monad (return))
import           Crypto.JOSE          (JWK, JWKSet (..))
import           Crypto.JWT           (StringOrURI, string, uri)
import qualified Data.Aeson           as Aeson
import           Data.ByteString      (readFile)
import qualified Data.Text            as T
import           Network.URI          (parseURI)
import           RIO                  (Eq ((==)), IO, Maybe (Just, Nothing),
                                       Semigroup ((<>)), const, either, error,
                                       fromMaybe, id, maybe, ($), (++))
import           Servant.Auth.Server  (IsMatch (..), JWTSettings (..),
                                       generateKey)
import           System.Environment   (lookupEnv)


getJWTAuthSettings :: IO JWTSettings
getJWTAuthSettings = do
  jwkSet <- acquireJwks
  signKey <- generateKey
  audienceCfg <- lookupEnv "JWK_AUDIENCES"
  let audMatches = maybe (const Matches) checkAud audienceCfg
      checkAud audConfig = \tokenAud ->
        if preview uri tokenAud == parseURI audConfig then
          Matches
        else if preview string tokenAud == Just (T.pack audConfig) then
          Matches
        else
          DoesNotMatch
  return $ buildJWTSettings signKey jwkSet audMatches

buildJWTSettings :: JWK -> JWKSet -> (StringOrURI -> IsMatch) -> JWTSettings
buildJWTSettings signKey jwkSet audMatches =
  JWTSettings
    { signingKey = signKey,
      jwtAlg = Nothing,
      validationKeys = vkeys signKey jwkSet,
      audienceMatches = audMatches
    }
  where
    vkeys k (JWKSet x) = JWKSet (x ++ [k])
acquireJwks :: IO JWKSet
acquireJwks = do
  envUrl <- lookupEnv "JWK_PATH"
  let jwkPath = fromMaybe "secrets/jwk.sig" envUrl
  fileContent <- readFile jwkPath
  let parsed = Aeson.eitherDecodeStrict fileContent
  return $ either (\e -> error $ "Invalid JWK file: " <> e) id parsed
