{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Authentication where

import           Servant
import           Data.Authentication

type PublicApi = "public" :> Get '[JSON] [PublicData]
type PrivateApi = "private" :> BasicAuth "foo-realm" User :> Get '[JSON] PrivateData
type AuthApi = PublicApi :<|> PrivateApi 
