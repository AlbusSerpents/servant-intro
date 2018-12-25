{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.UsersApi
    ( usersApi
    , UsersApi
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
