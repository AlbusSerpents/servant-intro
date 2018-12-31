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

module Server.Secured 
    ( basicAuthServer
    ) where

import qualified Data.Text as T
import           Servant

import           Data.Authentication
import           Api.Secured

publicHandler :: Handler [PublicData]
publicHandler = return $ map PublicData ["foo", "bar", "baz"]

privateHandler :: User -> Handler PrivateData
privateHandler  = return . PrivateData . T.append "Shhhhh " . userName

generalHandler :: Account -> Handler PrivateData
generalHandler = return . PrivateData . flip T.append ", you are protected " . unAccount

basicAuthServer :: Server AuthApi
basicAuthServer = publicHandler :<|> privateHandler :<|> generalHandler
