{-# LANGUAGE DeriveGeneric #-}

module Data.Dummy where

import           GHC.Generics

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving (Show, Generic)

newtype HelloMessage = HelloMessage { msg :: String }
      deriving (Show, Generic)

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving (Show, Generic)
