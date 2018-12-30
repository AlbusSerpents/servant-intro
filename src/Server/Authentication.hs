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

module Server.Authentication 
    ( basicAuthServer
    ) where

import qualified Data.Text as T
import           Servant

import           Data.Authentication
import           Api.Authentication

publicHandler :: Handler [PublicData]
publicHandler = return $ map PublicData ["foo", "bar", "baz"]

privateHandler :: User -> Handler PrivateData
privateHandler = return . PrivateData . T.append "Shhhhh " . userName

basicAuthServer :: Server AuthApi
basicAuthServer = publicHandler :<|> privateHandler
