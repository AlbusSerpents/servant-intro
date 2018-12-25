{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Api 
    ( app
    ) where

import           Data.Attoparsec.ByteString
import           Data.ByteString (ByteString)
import           Data.String.Conversions
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