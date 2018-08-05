{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSBSim.Api
  ( XML, jsbsimdApi, JSBSimdApi, getScriptLink, jsbsimdApiLink
  , ResourceHeaders, ResourceCreateHeaders
  ) where

import Text.XML.HXT.Arrow.Pickle
import           Data.JSBSim.Types
import           Data.Proxy
import           Data.Text         (Text)
import           Servant.API
import           Network.HTTP.Media ((//))
import Data.JSBSim.Helper
import Data.Time.Clock

data XML

instance Accept XML where
  contentType _ = "application" // "xml"

instance XmlPickler a => MimeRender XML a where
  mimeRender = const samlToXML

instance XmlPickler a => MimeUnrender XML a where
  mimeUnrender = const xmlToSAML

type ResourceHeaders =
  '[ Header "ETag" Text
   , Header "Last-Modified" UTCTime
   , Header "Expires" UTCTime
   ]

type ResourceCreateHeaders =
  '[ Header "Location" Text
   , Header "ETag" Text
   ]

type GetScript t =
  "scripts" :>
  Header "If-None-Match" String :>
  Capture "collection" Text :>
  Capture "scriptName" JsbSimScriptName :>
  Get '[JSON, XML] (Headers ResourceHeaders (JsbSimScript Double))

type PostScript t =
  "scripts" :>
  Capture "collection" Text :>
  ReqBody '[XML, JSON] (JsbSimScript t) :>
  PostCreated '[PlainText]
    (Headers ResourceCreateHeaders NoContent)






type JSBSimdApi t =
  "scripts" :> Capture "collection" Text :> Get '[JSON] [ JsbSimScriptName ] :<|>
  GetScript t :<|>
  PostScript t



jsbsimdApi :: Proxy (JSBSimdApi Double)
jsbsimdApi = Proxy

jsbsimdApiLink :: (IsElem endpoint (JSBSimdApi Double), HasLink endpoint)
  => Proxy endpoint -> MkLink endpoint Link
jsbsimdApiLink = safeLink jsbsimdApi

getScriptLink :: Text -> Text -> Link
getScriptLink = jsbsimdApiLink (Proxy :: Proxy (GetScript Double))
