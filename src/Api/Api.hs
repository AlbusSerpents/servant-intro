{-# LANGUAGE TypeOperators #-}

module Api.Api ( Api ) where

import           Servant

import           Api.Users
import           Api.Static
import           Api.Dummy

type Api = UsersApi :<|> StaticApi  :<|> DummyApi