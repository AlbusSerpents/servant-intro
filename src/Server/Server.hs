{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Server 
    ( app 
    ) where

import           Servant

import           Api.Api
import           Server.Authentication.Basic
import           Server.Authentication.General

import           Server.Users
import           Server.Static
import           Server.Dummy
import           Server.Secured

import           Data.Authentication
import           Servant.Server.Experimental.Auth
import           Data.Proxy
import           Network.Wai

app :: Application
app = serveWithContext apiProxy authContext server

server :: Server Api
server = usersServer :<|> staticServer :<|> dummyServer :<|> basicAuthServer

apiProxy :: Proxy Api
apiProxy = Proxy

authContext :: Context (AuthHandler Request Account ': BasicAuthCheck User ': '[])
authContext = authHandler :. authCheck :. EmptyContext
