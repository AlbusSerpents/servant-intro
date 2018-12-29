module Server.Users ( usersServer ) where

import           Api.Users
import           Servant
import           Servant.Types.SourceT
import           Services.Users

usersServer :: Server UsersApi
usersServer = listHandler :<|> detailsHandler :<|> createHandler :<|> streamingHandler

listHandler :: Handler [User]
listHandler = return users

detailsHandler :: String -> Handler User
detailsHandler name = (return $ getUser name) >>= (maybe (throwError err404) return)

createHandler :: User -> Handler UserCreated
createHandler = return . createUser

streamingHandler :: Server StreamUsers
streamingHandler = return $ source users