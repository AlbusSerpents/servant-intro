{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module UsersApi
    ( usersApp
    ) where

import           Data.Aeson
import           Data.Attoparsec.ByteString
import           Data.ByteString (ByteString)
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
import           Servant
import qualified Data.Aeson.Parser

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

type ListUsers = "users" :> Get '[JSON] [User]
type UserDetails = "users" :> Capture "name" String :> Get '[JSON] User 

type UsersApi = ListUsers :<|> UserDetails

users :: [User]
users =
    [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

getUser :: String -> Maybe User
getUser search = listToMaybe $ filter (\u -> (name u) == search) users

usersApi :: Server UsersApi
usersApi = listHandler :<|> detailsHandler
    where
        listHandler :: Handler [User]
        listHandler = return users
        
        detailsHandler :: String -> Handler User
        detailsHandler name = (return $ getUser name) >>= (maybe notFound return)

        notFound :: Handler User
        notFound = throwError err404 { errBody = "No user with that name." }
    
userAPI :: Proxy UsersApi
userAPI = Proxy

usersApp :: Application
usersApp = serve userAPI usersApi