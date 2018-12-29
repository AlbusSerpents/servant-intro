{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client.Dummy
    ( makePosition
    , sayHi
    , generateEmail
    ) where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           Control.Monad

import           Client.Base

import           Api.Dummy
import           Data.Dummy

targetApi :: Proxy DummyApi
targetApi = Proxy

getPosition :: Int -> Int -> ClientM Position
hello :: Maybe String -> ClientM HelloMessage
marketing :: ClientInfo -> ClientM Email

getPosition :<|> hello :<|> marketing = client targetApi

makePosition :: IO ()
makePosition = do
    putStrLn "Please Enter coordinates"
    x:y:_ <- (map read . words) `fmap` getLine
    position <- runQuery (getPosition x y)
    either print print position

sayHi :: IO ()
sayHi = do
    putStr "What's your name? "
    name <- processName `fmap` getLine
    greet <- runQuery (hello name)
    either print (putStrLn . msg) greet

processName :: String -> Maybe String
processName [] = Nothing
processName x = Just x

generateEmail :: ClientInfo -> IO ()
generateEmail info = (runQuery $ marketing info) >>= (\resp -> either print presentEmail resp)

presentEmail :: Email -> IO ()
presentEmail (Email from to subject body) = do
    putStrLn $ "From: " ++ from
    putStrLn $ "To: " ++ to
    putStrLn $ "Subject: " ++ subject
    putStrLn ""
    putStrLn body