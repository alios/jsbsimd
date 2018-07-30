{-# LANGUAGE OverloadedStrings #-}

module Control.JSBSim.Server (jsbsimdApp) where

import           Control.JSBSim.Handler
import           Control.JSBSim.Types
import           Control.Monad.Trans.Resource
import           Data.JSBSim.Api
import           Data.JSBSim.Types
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets
import           Servant
import           Servant.Server



jsbsimdApp :: Application
jsbsimdApp  = websocketApp apiApp

apiServer :: Server JSBSimdApi
apiServer =
  getInstances :<|>
  getInstance :<|>
  postInstance :<|>
  serveDirectoryWebApp "static"

apiApp :: Application
apiApp = serve jsbsimdApi apiServer

websocketApp :: Middleware
websocketApp = websocketsOr co sa
  where co = defaultConnectionOptions
        sa pc = do
          rejectRequest pc "not implemented yet"
