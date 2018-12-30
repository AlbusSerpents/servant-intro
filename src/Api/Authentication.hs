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

module Api.Authentication where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Aeson
import qualified Data.Map as Map
import           Data.Proxy
import           GHC.Generics
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.API.BasicAuth
import           Servant.API.Experimental.Auth
import           Servant
import           Servant.Server
import           Servant.Server.Experimental.Auth

newtype PrivateData = PrivateData { ssshhh :: T.Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

newtype PublicData = PublicData { somedata :: T.Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

newtype User = User { userName :: T.Text }
  deriving (Eq, Show)

type PublicApi = "public" :> Get '[JSON] [PublicData]
type PrivateApi = "private" :> BasicAuth "foo-realm" User :> Get '[JSON] PrivateData
type BasicAPI = PublicApi :<|> PrivateApi 

authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck (return . authenticate)
    where
      authenticate :: BasicAuthData -> BasicAuthResult User
      authenticate (BasicAuthData uname@"servant" "server") = Authorized $ User $ TE.decodeUtf8 $ uname
      authenticate _                                        = Unauthorized

publicHandler :: Handler [PublicData]
publicHandler = return $ Prelude.map PublicData ["foo", "bar", "baz"]

privateHandler :: User -> Handler PrivateData
privateHandler = return . PrivateData . T.append "Shhhhh " . userName

basicAuthServer :: Server BasicAPI
basicAuthServer = publicHandler :<|> privateHandler

basicAuthApi :: Proxy BasicAPI
basicAuthApi = Proxy

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

basicAuthMain :: IO ()
basicAuthMain = do
  putStrLn "Serving on port: 8080 "
  run 8080 $ serveWithContext basicAuthApi basicAuthServerContext basicAuthServer