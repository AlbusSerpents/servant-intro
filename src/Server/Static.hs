module Server.Static ( staticServer ) where

import           Servant
import           Api.Static

staticServer :: Server StaticApi
staticServer = serveDirectoryWebApp "static-files"