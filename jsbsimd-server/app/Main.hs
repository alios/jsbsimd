{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.JSBSim.Server
import           Control.Lens.Operators
import           Network.TLS
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.Warp.Internal
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.ForceSSL
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger


runTLSApp :: Bool -> FilePath -> FilePath -> Application -> IO ()
runTLSApp debug cert key =
  let tlsCfg = (tlsSettings cert key) {
        tlsAllowedVersions = [TLS12]
        }
      warpCfg = defaultSettings {
        settingsHost = "*6"
        }
      middleware = logStdoutDev . gzip def . forceSSL
  in runTLS tlsCfg defaultSettings . middleware



main :: IO ()
main = runTLSApp True cert key jsbsimdApp
  where cfg = _JSBSimdConfig # ("cert.pem", "key.pem")
        cert = cfg ^. jsbsimdTLSCert
        key = cfg ^. jsbsimdTLSKey
