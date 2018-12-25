{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.StaticApi 
    ( staticApi
    , StaticApi
    ) where

import           Servant

type StaticApi = "static" :> Raw

staticApi :: Server StaticApi
staticApi = serveDirectoryWebApp "static-files"

