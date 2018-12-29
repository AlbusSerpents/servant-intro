{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Server.Dummy ( dummyServer ) where

import           Servant
import           Control.Monad (liftM2)
import           Data.List

import           Api.Dummy
import           Data.Dummy
import           Services.Dummy

dummyServer :: Server DummyApi
dummyServer = positionHandler
    :<|> (return . sayHello)
    :<|> (return . makeEmail)

positionHandler :: Int -> Int -> Handler Position
positionHandler x y = return $ makePosition x y