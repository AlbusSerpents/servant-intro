{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.UsersApi
    ( usersApi
    , UsersApi
    ) where

import           Servant
import           Services.Users

type ListUsers = "users" :> Get '[JSON] [User]
type UserDetails = "users" :> Capture "name" String :> Get '[JSON] User 

type UsersApi = ListUsers :<|> UserDetails

usersApi :: Server UsersApi
usersApi = listHandler :<|> detailsHandler
    where
        listHandler :: Handler [User]
        listHandler = return users
        
        detailsHandler :: String -> Handler User
        detailsHandler name = (return $ getUser name) >>= (maybe (throwError err404) return)
