{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Secured where

import           Servant
import           Data.Authentication
import           Servant.Server.Experimental.Auth

type PublicApi = "public" :> Get '[JSON] [PublicData]
type PrivateApi = "private" :> BasicAuth "foo-realm" User :> Get '[JSON] PrivateData
type GeneralAuthApi = "private" :> "general" :> AuthProtect "cookie-auth" :> Get '[JSON] PrivateData 

type instance AuthServerData (AuthProtect "cookie-auth") = Account

type AuthApi = PublicApi :<|> PrivateApi :<|> GeneralAuthApi