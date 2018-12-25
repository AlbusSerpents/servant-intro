{-# LANGUAGE DeriveGeneric #-}

module Services.Users 
    ( users
    , getUser
    , User
    ) where

import           Data.Aeson
import           Data.Time.Calendar
import           GHC.Generics
import           Data.Maybe

data User = User
    { name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
    } deriving (Eq, Show, Generic)
      
instance ToJSON User

users :: [User]
users =
    [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

getUser :: String -> Maybe User
getUser search = listToMaybe $ filter (\u -> (name u) == search) users
