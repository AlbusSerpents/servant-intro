{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Server.Server 
    ( app 
    ) where

import           Servant

import           Api.Api

import           Server.Users
import           Server.Static
import           Server.Dummy
import           Server.Authentication

import qualified Data.Authentication as A
import qualified Data.Text.Encoding as TE
import           Data.Proxy

server :: Server Api
server = usersServer :<|> staticServer :<|> dummyServer :<|> basicAuthServer

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serveWithContext apiProxy basicAuthServerContext server


authCheck :: BasicAuthCheck A.User
authCheck = BasicAuthCheck (return . authenticate)
    where
        authenticate :: BasicAuthData -> BasicAuthResult A.User
        authenticate (BasicAuthData uname@"servant" "server") = Authorized $ A.User $ TE.decodeUtf8 $ uname
        authenticate _                                        = Unauthorized

basicAuthServerContext :: Context (BasicAuthCheck A.User ': '[])
basicAuthServerContext = authCheck :. EmptyContext