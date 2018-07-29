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


runTLSApp :: HasJSBSimdConfig cfg => cfg -> Application -> IO ()
runTLSApp cfg =
  let tlsCfg = (tlsSettings cert key) {
        tlsAllowedVersions = [TLS12],
        onInsecure = AllowInsecure
        }
      warpCfg = defaultSettings {
        settingsHost = "*6"
        }
      middleware = logStdoutDev . forceSSL . gzip def
      cert = cfg ^. jsbsimdTLSCert
      key = cfg ^. jsbsimdTLSKey

  in runTLS tlsCfg defaultSettings . middleware



main :: IO ()
main = runTLSApp cfg jsbsimdApp
  where cfg = _JSBSimdConfig # (True, "cert.pem", "key.pem")
