{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Domain.API.User (server, UserAPI, userAPI, newInMemRepo) where

import qualified Data.HashMap.Strict as B
import           Domain.Types
import           Import
import           Servant
import           Servant.Auth.Server as SAS

type UserAPI =
  "users"
    :> ( Get '[JSON] [User]
           :<|> Capture "id" Text :> Get '[JSON] (Maybe User)
           :<|> ReqBody '[JSON] User :> Post '[JSON] NoContent
       )

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: AuthResult val -> App [User] :<|> ((Text -> App (Maybe User)) :<|> (User -> App NoContent))
server (SAS.Authenticated user) = getUsers :<|> findUserById :<|> createUser
server _ = throwUnauthorized :<|> const throwUnauthorized :<|> const throwUnauthorized

newInMemRepo :: (MonadUnliftIO m) => m UserRepo
newInMemRepo = do
  store <- liftIO $ atomically $ newTVar (B.fromList defaultUsers)
  return $
    UserRepo
      { _getUser = \uid -> do
          u <- atomically $ readTVar store
          return $ B.lookup uid u,
        _getAllUsers = do
          u <- atomically $ readTVar store
          return $ B.elems u,
        _insertUser = \uid u -> do
          lst <- atomically $ readTVar store
          atomically $ writeTVar store (B.insert uid u lst)
          return ()
      }

defaultUsers :: [(Text, User)]
defaultUsers = [("1234", User (Name "name1") (Email "email@test.com"))]

getUsers :: App [User]
getUsers = do
  logWarn "Fetching users from db"
  repo <- askObj
  users <- liftIO $ _getAllUsers repo
  return users

findUserById :: Text -> App (Maybe User)
findUserById userId = do
  logInfoS "find" ("Find by id = " <> display userId)
  repo <- askObj
  mu <- liftIO $ _getUser repo $ userId
  return mu

createUser :: User -> App NoContent
createUser u = do
  logInfoS "create" "New user created"
  repo <- askObj
  liftIO $ _insertUser repo "33" u
  return NoContent
