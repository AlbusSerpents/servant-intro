{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client.MockServer ( clientApi ) where

import           Servant
import           Client.ClientApi
import           Control.Monad (liftM2)
import           Data.List

clientApi :: Server ClientApi
clientApi =  positionHandler
    :<|> (return . helloHandler)
    :<|> (return . marketingHandler)

positionHandler :: Int -> Int -> Handler Position
positionHandler x y = return $ Position x y

helloHandler :: Maybe String -> HelloMessage
helloHandler =  HelloMessage . ("Hello " ++) . maybe "Anonymus" id

marketingHandler :: ClientInfo -> Email
marketingHandler (ClientInfo name email age interests) = Email marketingEmail email subject body
    where
        marketingEmail = "marketing@co.com"
        subject = "Please come back"
        body = "   Hello, " ++ name ++ "\r\n\r\n"
            ++ " We would like you see if you are still insteresed in: \r\n"
            ++ " " ++ (concat $ intersperse ", " interests) ++ "\r\n"
            ++ " If you are, please respond to this email.\r\n"
            ++ " Thank you, \r\n"
            ++ " Bye"