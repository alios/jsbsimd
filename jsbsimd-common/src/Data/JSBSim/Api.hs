{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSBSim.Api
  ( XML, jsbsimdApi, JSBSimdApi, getScriptLink, jsbsimdApiLink
  , ResourceHeaders, ResourceCreateHeaders, ResourceUpdateHeaders
  ) where

import Control.Monad.Catch
import           Data.JSBSim.Types
import           Data.Proxy
import           Data.Text         (Text)
import           Servant.API
import           Network.HTTP.Media ((//))
import Data.JSBSim.Helper
import Data.Time.Clock
import qualified Text.XML.Stream.Render as XR
import qualified Text.XML.Stream.Parse as XP
import qualified Data.Conduit.List as CL
import Conduit
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Builder
import  Data.JSBSim.XML
import  Data.JSBSim.Script



type ResourceHeaders =
  '[ Header "ETag" Text
   , Header "Last-Modified" UTCTime
   , Header "Expires" UTCTime
   ]

type ResourceCreateHeaders =
  '[ Header "Location" Text
   , Header "ETag" Text
   ]

type ResourceUpdateHeaders = '[ Header "ETag" Text ]

type GetScript t =
  Header "If-None-Match" String :>
  "scripts" :>
  Capture "collection" Text :>
  Capture "scriptName" Text :>
  Get '[JSON, XML] (Headers ResourceHeaders (JsbSimScript Double))

type PostScript t =
  "scripts" :>
  Capture "collection" Text :>
  ReqBody '[XML, JSON] (JsbSimScript t) :>
  PostCreated '[PlainText]
    (Headers ResourceCreateHeaders NoContent)

type PutScript t =
  Header "If-Match" String :>
  "scripts" :>
  Capture "collection" Text :>
  Capture "scriptName" Text :>
  ReqBody '[XML, JSON] (JsbSimScript t) :>
  Put '[PlainText]
    (Headers ResourceUpdateHeaders NoContent)





type JSBSimdApi t =
  "scripts" :> Capture "collection" Text :> Get '[JSON] [ Text ] :<|>
  GetScript t :<|>
  PostScript t :<|>
  PutScript t



jsbsimdApi :: Proxy (JSBSimdApi Double)
jsbsimdApi = Proxy

jsbsimdApiLink :: (IsElem endpoint (JSBSimdApi Double), HasLink endpoint)
  => Proxy endpoint -> MkLink endpoint Link
jsbsimdApiLink = safeLink jsbsimdApi

getScriptLink :: Text -> Text -> Link
getScriptLink = jsbsimdApiLink (Proxy :: Proxy (GetScript Double))
