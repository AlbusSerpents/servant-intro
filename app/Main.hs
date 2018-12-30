module Main where

import           Server.Server (app)
import           Network.Wai.Handler.Warp

main :: IO ()
main = do
    putStrLn $ "Running on port " ++ show port
    run port app
    where
        port = 8080