module Main where

import           Api.Api
import           Network.Wai.Handler.Warp

main :: IO ()
main = run 8081 app
    