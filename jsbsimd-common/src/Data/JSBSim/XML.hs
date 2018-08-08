{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.JSBSim.XML (XML, MonadParseXML, FromXML(..), ToXML(..)) where

import           Conduit
import           Control.Monad.Catch
import           Data.Binary.Builder
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Conduit.List      as CL
import           Data.Proxy
import           Data.Text              (Text)
import           Data.Time.Clock
import           Data.XML.Types
import           Network.HTTP.Media     ((//))
import           Servant.API
import qualified Text.XML.Stream.Parse  as XP
import qualified Text.XML.Stream.Render as XR


data XML

type MonadParseXML m = (MonadThrow m)

class FromXML a where
  fromXML :: MonadParseXML m => ConduitT Event Void m (Maybe a)

class ToXML a where
  toXML :: Monad m => a -> ConduitT () Event m ()


instance Accept XML where
  contentType _ = "application" // "xml"

instance ToXML a => MimeRender XML a where
  mimeRender _ a = toLazyByteString . mconcat . runConduitPure $
    ( toXML a .| XR.renderBuilder (XR.def) .| CL.consume)

instance FromXML a => MimeUnrender XML a where
  mimeUnrender _ bs = case runConduit $ XP.parseLBS (XP.def) bs .| fromXML of
    Left e         -> Left . show $ e
    Right Nothing  -> Left "unable to parse xml"
    Right (Just a) -> Right a
