{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.UsersApi
    ( usersApi
    , UsersApi
    ) where

import           Data.Aeson
import           Servant
import           Services.Users

instance ToJSON UserCreated

instance ToJSON User
instance FromJSON User

type ListUsers = "users" :> Get '[JSON] [User]
type UserDetails = "users" :> Capture "name" String :> Get '[JSON] User 
type CreateUser = "users" :> ReqBody '[JSON] User :> Post '[JSON] UserCreated

type UsersApi = ListUsers :<|> UserDetails :<|> CreateUser

usersApi :: Server UsersApi
usersApi = listHandler :<|> detailsHandler :<|> createHandler

listHandler :: Handler [User]
listHandler = return users

detailsHandler :: String -> Handler User
detailsHandler name = (return $ getUser name) >>= (maybe (throwError err404) return)

createHandler :: User -> Handler UserCreated
createHandler = return . createUser
