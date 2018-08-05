{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Control.JSBSim.Server (jsbsimdApp, allocateConfig, allocateState) where

import           Control.JSBSim.Database
import           Control.JSBSim.Handler
import           Control.JSBSim.Types
import           Control.Lens.Getter
import           Control.Lens.Operators
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.JSBSim.Api
import qualified Data.Text                       as T
import           Database.MongoDB.Connection
import           Database.MongoDB.Query
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets
import           Servant
import           System.Directory
import           System.Posix.Temp
import           WaiAppStatic.Storage.Filesystem
import           WaiAppStatic.Types


jsbsimdApp :: HasSimdState st => st ->  Application
jsbsimdApp st0 = websocketApp st0 (apiApp st0)


websocketApp :: HasSimdState st => st -> Middleware
websocketApp st = websocketsOr co sa
  where co = defaultConnectionOptions
        sa pc = do
          rejectRequest pc "not implemented yet"


type JSBSimdApiRaw = (JSBSimdApi Double) :<|> Raw

apiServer :: ServerT (JSBSimdApi Double) SimdM
apiServer =
  getScripts :<|>
  getScript :<|>
  postScript

apiStaticServer :: ServerT JSBSimdApiRaw SimdM
apiStaticServer = apiServer :<|> serveDirectoryWith (staticCfg "static")


apiApp :: HasSimdState st => st -> Application
apiApp = serve jsbsimdApiRaw . srv
  where srv st = hoistServer jsbsimdApiRaw (nt st) apiStaticServer
        nt a = flip runReaderT (view simdState a) . runStdoutLoggingT


jsbsimdApiRaw :: Proxy JSBSimdApiRaw
jsbsimdApiRaw = Proxy



staticCfg :: FilePath -> StaticSettings
staticCfg fp =
  let d = defaultWebAppSettings fp
  in d {
    ssGetMimeType = \fp' -> if
        (T.isSuffixOf ".webmanifest" . fromPiece . fileName $ fp')
        then pure "application/manifest+json"
        else if (T.isSuffixOf ".xsl" . fromPiece . fileName $ fp')
             then pure "text/xsl"
             else ssGetMimeType d fp'
    }


allocateTempDir :: MonadResource m => m FilePath
allocateTempDir = do
  fp <- snd <$> allocate (mkdtemp "jsbsimd.tmp") removeDirectoryRecursive
  return fp

allocateConfig ::
  MonadResource m =>
  Port
  -> Bool
  -> FilePath
  -> FilePath
  -> MongoConfig String
  -> Database
  -> m SimdConfig
allocateConfig p d c k m db = do
  tmpdir <- allocateTempDir
  m' <- lookupHosts m
  return $ _SimdConfig # (p, d, c, k, tmpdir, m', db)

lookupHosts :: Monad m => MongoConfig String -> m (MongoConfig Host)
lookupHosts (Right (rsn, hs)) = do
  hs' <- sequence $ readHostPortM <$> hs
  return (Right (rsn, hs'))
lookupHosts (Left h) = Left <$> readHostPortM h

allocateState :: (HasSimdConfig cfg, MonadResource m) => cfg -> m SimdState
allocateState cfg = do
  rs <- allocateMongoPipes cfg
  pure $ _SimdState # (cfg ^. simdConfig, rs)
