{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Dummy where

import           Servant
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics

import           Servant.API
import           Servant.Client
import           Servant.Types.SourceT (foreach)

import           Data.Dummy

instance FromJSON Position
instance ToJSON Position

instance FromJSON HelloMessage
instance ToJSON HelloMessage

instance ToJSON ClientInfo
instance FromJSON ClientInfo

instance FromJSON Email
instance ToJSON Email

type DummyApi = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email