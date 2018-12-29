{-# LANGUAGE DeriveGeneric #-}

module Data.User where

import           GHC.Generics
import           Data.Time.Calendar

data UserCreated = Created 
    { id :: Int } deriving (Eq, Show, Generic)

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    } deriving (Eq, Show, Generic)
