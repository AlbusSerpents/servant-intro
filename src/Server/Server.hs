module Server.Server where

import           Servant

import           Api.Api

import           Server.Users
import           Server.Static
import           Server.Dummy

api :: Server Api
api = usersServer :<|> staticServer :<|> dummyServer

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy api