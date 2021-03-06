module Client.Base 
    ( runQuery
    , queryStream
    ) where

import           Servant.Client
import           Servant.Types.SourceT (foreach)
import qualified Servant.Client.Streaming as S
import           Network.HTTP.Client (newManager, defaultManagerSettings)

runQuery :: ClientM a -> IO (Either ServantError a)
runQuery query = environment >>= (runClientM  query)
    
environment :: IO ClientEnv
environment = do 
    manager <- newManager defaultManagerSettings
    return $ mkClientEnv manager baseUrl
    where
        baseUrl = BaseUrl Http "localhost" 8080 ""

queryStream :: S.ClientM a -> (Either ServantError a -> IO b) -> IO b
queryStream req handler = environment >>= (\e -> S.withClientM req e handler)