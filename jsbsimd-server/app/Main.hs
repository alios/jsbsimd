{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.JSBSim
import           Control.Lens.Operators
import           Network.TLS
import           Network.Wai
import           Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Handler.Warp.Internal
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.Brotli        as Brotli
import           Network.Wai.Middleware.ForceSSL
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger

runTLSApp :: HasJSBSimdConfig cfg => cfg -> Application -> IO ()
runTLSApp cfg =
  let tlsCfg = (tlsSettings cert key) {
        tlsAllowedVersions = [TLS12],
        onInsecure = AllowInsecure
        }
      warpCfg = Warp.defaultSettings {
        settingsHost = "*6"
        }
      brotliCfg = Brotli.defaultSettings {
        brotliFilesBehavior = BrotliCompress
        }
      gzipCfg = def {
        gzipFiles = GzipCompress

        }
      middleware = logStdoutDev . forceSSL . gzip gzipCfg . brotli brotliCfg
      cert = cfg ^. jsbsimdTLSCert
      key = cfg ^. jsbsimdTLSKey

  in runTLS tlsCfg warpCfg . middleware



main :: IO ()
main = runTLSApp cfg jsbsimdApp
  where cfg = _JSBSimdConfig # (3000, True, "cert.pem", "key.pem")
