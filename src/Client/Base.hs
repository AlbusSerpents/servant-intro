module Client.Base 
    ( runQuery
    ) where

import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant.Client
import           Servant.Types.SourceT (foreach)

runQuery :: ClientM a -> IO (Either ServantError a)
runQuery query = environment >>= (runClientM  query)
    
environment :: IO ClientEnv
environment = do 
    manager <- newManager defaultManagerSettings
    return $ mkClientEnv manager baseUrl
    where
        baseUrl = BaseUrl Http "localhost" 8081 ""
