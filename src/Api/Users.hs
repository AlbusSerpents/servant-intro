{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users where

import           Data.Aeson
import           Servant
import           Services.Users
import           Servant.Types.SourceT

instance ToJSON UserCreated
instance FromJSON UserCreated

instance ToJSON User
instance FromJSON User

type ListUsers = "users" :> Get '[JSON] [User]
type GetUser = "users" :> Capture "name" String :> Get '[JSON] User 
type CreateUser = "users" :> ReqBody '[JSON] User :> Post '[JSON] UserCreated
type StreamUsers = "users" :> "stream" :> "present" :> StreamGet NewlineFraming JSON (SourceIO User)

type UsersApi = ListUsers :<|> GetUser :<|> CreateUser :<|> StreamUsers