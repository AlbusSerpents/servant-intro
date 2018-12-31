{-# LANGUAGE OverloadedStrings     #-}

module Server.Authentication.Basic 
    ( authCheck
    ) where

import           Servant

import           Data.Authentication
import           Data.Text.Encoding

authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck (return . authenticate)

authenticate :: BasicAuthData -> BasicAuthResult User
authenticate (BasicAuthData uname@"servant" "server") = Authorized $ User $ decodeUtf8 $ uname
authenticate _                                        = Unauthorized
