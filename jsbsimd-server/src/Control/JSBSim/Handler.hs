module Control.JSBSim.Handler where


import           Control.JSBSim.Database
import           Control.JSBSim.Types
import           Control.Monad.Reader
import           Data.JSBSim.Types
import           Servant



type SimdM = ReaderT SimdState Handler


getInstances :: SimdM [SimulatorInstance]
getInstances = do
  writeTestData
  return mempty

getInstance :: SimulatorInstanceId -> SimdM SimulatorInstance
getInstance = undefined

postInstance :: SimulatorInstanceId -> SimulatorInstanceCommand -> SimdM NoContent
postInstance = undefined
