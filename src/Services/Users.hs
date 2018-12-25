{-# LANGUAGE DeriveGeneric #-}

module Services.Users 
    ( users
    , getUser
    , createUser
    , User
    , UserCreated
    ) where

import           Data.Time.Calendar
import           Data.Maybe
import           GHC.Generics

data UserCreated = Created 
    { id :: Int } deriving (Eq, Show, Generic)

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    } deriving (Eq, Show, Generic)

users :: [User]
users =
    [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

getUser :: String -> Maybe User
getUser search = listToMaybe $ filter (\u -> (name u) == search) users

createUser :: User -> UserCreated
createUser (User name age email registrationDate) = Created $ age * (length name)