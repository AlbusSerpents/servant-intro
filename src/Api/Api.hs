{-# LANGUAGE TypeOperators #-}

module Api.Api 
    ( app
    ) where

import           Api.UsersApi
import           Api.StaticApi
import           Servant

type Api = UsersApi :<|> StaticApi

api :: Server Api
api = usersApi :<|> staticApi

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy api