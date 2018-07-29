{-# LANGUAGE TemplateHaskell #-}

module Control.JSBSim.Types
  ( _JSBSimdConfig, HasJSBSimdConfig(..)
  ) where

import           Control.Lens.TH
import           Network.Wai.Handler.Warp


data JSBSimdConfig = JSBSimdConfig {
  _jsbsimdHTTPPort :: Port,
  _jsbsimdDebug    :: Bool,
  _jsbsimdTLSCert  :: FilePath,
  _jsbsimdTLSKey   :: FilePath
  } deriving (Show, Eq)

makePrisms ''JSBSimdConfig
makeClassy ''JSBSimdConfig
