{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Server.Authentication.General
    ( authHandler
    ) where

import           Servant

import qualified Data.Authentication as A

import qualified Data.ByteString as BS
import qualified Data.Map as M

import           Servant.Server.Experimental.Auth
import           Network.Wai

authHandler :: AuthHandler Request A.Account
authHandler = mkAuthHandler handler
  where
  throw401 msg = throwError $ err401 { errBody = msg }
  handler req = either throw401 lookupAccount $ maybeToEither "Access Denied" $ getCookie req

database :: M.Map BS.ByteString A.Account
database = M.fromList [ ("key1", A.Account "Anne Briggs")
                    , ("key2", A.Account "Bruce Cockburn")
                    , ("key3", A.Account "Ghédalia Tazartès")
                    ]

lookupAccount :: BS.ByteString -> Handler A.Account
lookupAccount = maybe (throwError $ err403 {errBody = "Invalid credentials"}) return . flip M.lookup database
  
getCookie :: Request -> Maybe BS.ByteString
getCookie req = (return $ requestHeaders req) >>= (lookup "x-auth-token")

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right
