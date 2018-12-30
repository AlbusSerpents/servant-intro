{-# LANGUAGE TypeOperators #-}

module Api.Api ( Api ) where

import           Servant

import           Api.Users
import           Api.Static
import           Api.Dummy
import           Api.Authentication

type Api = UsersApi :<|> StaticApi  :<|> DummyApi :<|> AuthApi