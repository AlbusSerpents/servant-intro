{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.UsersStreamingApi
    ( streamingApi
    , UsersStreamingApi
    ) where

import           Control.Monad.IO.Class        
import           Services.Users
import           Data.Aeson
import           Data.Maybe
import           Servant
import           Servant.Types.SourceT

type UsersStreamingApi = "users" :> "stream" :> "present" :> StreamGet NewlineFraming JSON (SourceIO User)

streamingApi :: Server UsersStreamingApi
streamingApi = return $ source (users)