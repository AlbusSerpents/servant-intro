{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Static ( StaticApi ) where

import           Servant

type StaticApi = "static" :> Raw