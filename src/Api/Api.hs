{-# LANGUAGE TypeOperators #-}

module Api.Api 
    ( app
    ) where

import           Api.UsersApi
import           Api.StaticApi
import           Api.UsersStreamingApi
import           Servant

type Api = UsersApi :<|> StaticApi :<|> UsersStreamingApi

api :: Server Api
api = usersApi :<|> staticApi :<|> streamingApi

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy api