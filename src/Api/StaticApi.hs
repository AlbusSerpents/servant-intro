{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.StaticApi 
    ( staticApi
    , StaticApi
    ) where

import           Data.Attoparsec.ByteString
import           Data.ByteString (ByteString)
import           Data.String.Conversions
import           Servant

type StaticApi = "static" :> Raw

staticApi :: Server StaticApi
staticApi = serveDirectoryWebApp "static-files"

