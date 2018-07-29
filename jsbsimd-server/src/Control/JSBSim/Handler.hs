module Control.JSBSim.Handler where


import           Data.JSBSim.Types
import           Servant

getInstances :: Handler [SimulatorInstance]
getInstances = return mempty

getInstance :: SimulatorInstanceId -> Handler SimulatorInstance
getInstance = undefined

postInstance :: SimulatorInstanceId -> SimulatorInstanceCommand -> Handler NoContent
postInstance = undefined
