{-# LANGUAGE DeriveGeneric         #-}

module Data.Authentication where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

newtype PrivateData = PrivateData { ssshhh :: Text }
    deriving (Eq, Show, Generic)

instance ToJSON PrivateData

newtype PublicData = PublicData { somedata :: Text }
    deriving (Eq, Show, Generic)

instance ToJSON PublicData

newtype User = User { userName :: Text }
    deriving (Eq, Show)

newtype Account = Account { unAccount :: Text }