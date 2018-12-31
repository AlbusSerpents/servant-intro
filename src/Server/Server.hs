{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

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
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as M
import           Servant.Server.Experimental.Auth
import           Network.Wai
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

basicAuthServerContext :: Context (AuthHandler Request A.Account ': BasicAuthCheck A.User ': '[])
basicAuthServerContext = authHandler :. authCheck :. EmptyContext

database :: M.Map BS.ByteString A.Account
database = M.fromList [ ("key1", A.Account "Anne Briggs")
                    , ("key2", A.Account "Bruce Cockburn")
                    , ("key3", A.Account "Ghédalia Tazartès")
                    ]

lookupAccount :: BS.ByteString -> Handler A.Account
lookupAccount = maybe (throwError $ err403 {errBody = "Invalid credentials"}) return . flip M.lookup database

authHandler :: AuthHandler Request A.Account
authHandler = mkAuthHandler handler
  where
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req = either throw401 lookupAccount $ maybeToEither "Access Denied" $ getCookie req
  
getCookie :: Request -> Maybe BS.ByteString
getCookie req = (return $ requestHeaders req) >>= (lookup "x-auth-token")

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right
