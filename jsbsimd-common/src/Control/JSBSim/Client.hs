{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Control.JSBSim.Client (apiClient) where


import           Data.JSBSim.Api
import           Data.JSBSim.Types
import           Data.Proxy
import           Servant.API
import           Servant.Client.Core

data JSBSimdClient m = JSBSimdClient
  { getInstances  :: m [SimulatorInstance]
  , getInstance   :: SimulatorInstanceId -> m SimulatorInstance
  , postInstance  :: SimulatorInstanceId -> SimulatorInstanceCommand -> m NoContent
  }

apiClient
    :: forall m
     . RunClient m
    => JSBSimdClient m
apiClient = JSBSimdClient { .. }
  where
    getInstances
      :<|> getInstance
      :<|> postInstance = Proxy @JSBSimdApi `clientIn` Proxy @m
