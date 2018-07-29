{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Data.JSBSim.Api (jsbsimdApi, JSBSimdApi, WithInstanceId)  where

import           Data.JSBSim.Types
import           Data.Proxy
import           Servant.API



type WithInstanceId = Capture "instanceId" SimulatorInstanceId

type JSBSimdApi
  =     "instances"  :> Get '[JSON] [ SimulatorInstance ]
  :<|>  "instances" :>  WithInstanceId :> Get '[JSON] SimulatorInstance
  :<|> "instances" :> WithInstanceId :> ReqBody '[JSON] SimulatorInstanceCommand :>
        Post '[JSON] NoContent
  :<|> Raw

jsbsimdApi :: Proxy JSBSimdApi
jsbsimdApi = Proxy
