module Main where

import           UsersApi
import           Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 usersApp
    