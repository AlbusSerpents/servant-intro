{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client.UserClient where

import           Data.Aeson
import           Data.Proxy
import           Servant.API
import           Servant
import           Servant.Client
import           Control.Monad
import           Data.Time.Calendar

import qualified Servant.Client.Streaming as S
import           Servant.Types.SourceT (foreach)    

import           Client.Base
import           Api.UsersApi
import           Api.UsersStreamingApi
import qualified Services.Users as U

instance FromJSON U.UserCreated

streamUsers :: S.ClientM (SourceIO U.User)
streamUsers = S.client (Proxy :: Proxy UsersStreamingApi)

list :: ClientM [U.User]
details :: String -> ClientM U.User
create :: U.User -> ClientM U.UserCreated
list :<|> details :<|> create = client (Proxy :: Proxy UsersApi)

usersPresent :: IO ()
usersPresent = (runQuery list) >>= (either print (flip forM_ print))

userDetails :: String -> IO ()
userDetails name = do
    result <- runQuery $ details name
    either print print result

clientMakeUser :: String -> String -> IO U.UserCreated
clientMakeUser name email = do
    res <- runQuery $ create user
    either handleErorr return res
    where
        user = U.User name 8 email $ fromGregorian 2010 1 21
        handleErorr err = do
            print err
            return $ U.Created 1

data UserView = UserView 
    { name :: String
    , age :: Int 
    } deriving (Eq, Show)

getUsersStream :: IO ()
getUsersStream = queryStream streamUsers streamHandler

streamHandler :: Either ServantError (SourceIO U.User) -> IO ()
streamHandler (Left err) = putStrLn $ "Error" ++ (show err)
streamHandler (Right users) = foreach fail (putStrLn . show . transform) users
    where
        transform u = UserView (U.name u) (U.age u)