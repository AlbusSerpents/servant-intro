module Services.Users 
    ( users
    , getUser
    , createUser
    , User(..)
    , UserCreated(..)
    ) where

import           Data.Maybe (listToMaybe)
import           Data.User
import           Data.Time.Calendar (fromGregorian)

users :: [User]
users =
    [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

getUser :: String -> Maybe User
getUser search = listToMaybe $ filter (\u -> (name u) == search) users

createUser :: User -> UserCreated
createUser (User name age email registrationDate) = Created $ age * (length name)