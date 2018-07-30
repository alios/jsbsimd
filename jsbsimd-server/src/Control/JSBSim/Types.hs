{-# LANGUAGE TemplateHaskell #-}

module Control.JSBSim.Types
  ( SimdConfig, _SimdConfig, HasSimdConfig(..)
  , SimdState, _SimdState, HasSimdState(..)
  , MongoConfig
  ) where

import           Control.Lens.TH
import           Database.MongoDB.Connection
import           Database.MongoDB.Query
import           Network.Wai.Handler.Warp


type MongoConfig t = Either t (ReplicaSetName, [t])

data SimdConfig = SimdConfig {
  _simdHTTPPort      :: Port,
  _simdDebug         :: Bool,
  _simdTLSCert       :: FilePath,
  _simdTLSKey        :: FilePath,
  _simdTempDir       :: FilePath,
  _simdMongoConfig   :: MongoConfig Host,
  _simdMongoDatabase :: Database
  } deriving (Show, Eq)
makePrisms ''SimdConfig
makeClassy ''SimdConfig

data SimdState = SimdState {
  _simdServerConfig :: SimdConfig,
  _simdMongo        :: (Pipe, Pipe)
  }

makePrisms ''SimdState
makeClassy ''SimdState

instance HasSimdConfig SimdState where
  simdConfig = simdServerConfig
