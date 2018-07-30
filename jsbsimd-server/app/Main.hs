{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.JSBSim
import           Control.Lens.Operators
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Network.TLS
import           Network.Wai
import           Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Handler.Warp.Internal
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.Brotli        as Brotli
import           Network.Wai.Middleware.ForceSSL
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger


runTLSApp :: (HasSimdConfig cfg, MonadIO m) => cfg -> Application -> m ()
runTLSApp cfg =
  let d = cfg ^. simdDebug
      tlsCfg = (tlsSettings cert key) {
        tlsAllowedVersions = [TLS12],
        onInsecure = AllowInsecure
        }
      warpCfg = Warp.defaultSettings {
        settingsHost = "*6"
        }
      brotliCfg = Brotli.defaultSettings {
        brotliFilesBehavior = if d then BrotliCompress else BrotliCacheFolder tmpdir
        }
      gzipCfg = def {
        gzipFiles = if d then GzipCompress else GzipCacheFolder tmpdir
        }
      middleware = logStdoutDev . forceSSL . gzip gzipCfg . brotli brotliCfg
      cert = cfg ^. simdTLSCert
      key = cfg ^. simdTLSKey
      tmpdir = cfg ^. simdTempDir
  in liftIO . runTLS tlsCfg warpCfg . middleware



main :: IO ()
main = runResourceT $ do
  cfg <- allocateConfig 3000 True "cert.pem" "key.pem" (Left "localhost") "jsbsimd"
  st <- allocateState cfg
  runTLSApp cfg (jsbsimdApp st)
