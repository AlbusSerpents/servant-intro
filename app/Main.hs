module Main where

import           Server.Server (app)
import           Network.Wai.Handler.Warp

main :: IO ()
main = do
    putStrLn $ "Running on port " ++ show 8081
    run 8081 app