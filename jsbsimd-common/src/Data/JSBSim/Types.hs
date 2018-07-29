{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.JSBSim.Types where

import           Control.Lens.Operators
import           Control.Lens.TH
import           Data.Aeson
import qualified Data.Text              as T
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import           Servant.API
import           Text.Read

newtype SimulatorInstanceId = SimulatorInstanceId Int
  deriving (Show, Eq, Ord, Typeable, Generic)

makePrisms ''SimulatorInstanceId
makeClassy ''SimulatorInstanceId

data SimulatorInstance = SimulatorInstance  {
  _simulatorId :: SimulatorInstanceId
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''SimulatorInstance
makePrisms ''SimulatorInstance

instance HasSimulatorInstanceId SimulatorInstance where
  simulatorInstanceId = simulatorId

data SimulatorInstanceCommand
  = SimulatorHalt
  | SimulatorRun
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON SimulatorInstanceCommand
instance FromJSON SimulatorInstanceCommand

instance ToJSON SimulatorInstanceId
instance FromJSON SimulatorInstanceId

instance ToJSON SimulatorInstance
instance FromJSON SimulatorInstance

instance FromHttpApiData SimulatorInstanceId where
  parseUrlPiece a =
    maybe (Left a) Right . fmap SimulatorInstanceId . readMaybe . T.unpack $ a


t = toJSON $ _SimulatorInstance # (_SimulatorInstanceId # 23)
