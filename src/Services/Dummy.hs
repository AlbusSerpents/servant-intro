module Services.Dummy where

import           Control.Monad (liftM2)
import           Data.List

import           Api.Dummy
import           Data.Dummy

makePosition :: Int -> Int -> Position
makePosition x y = Position x y

sayHello :: Maybe String -> HelloMessage
sayHello =  HelloMessage . ("Hello " ++) . maybe "Anonymus" id

makeEmail :: ClientInfo -> Email
makeEmail (ClientInfo name email age interests) = Email marketingEmail email subject body
    where
        marketingEmail = "marketing@co.com"
        subject        = "Please come back"
        body           = "   Hello, " ++ name ++ "\r\n\r\n"
                      ++ " We would like you see if you are still insteresed in: \r\n"
                      ++ " " ++ (concat $ intersperse ", " interests) ++ "\r\n"
                      ++ " If you are, please respond to this email.\r\n"
                      ++ " Thank you, \r\n"
                      ++ " Bye"