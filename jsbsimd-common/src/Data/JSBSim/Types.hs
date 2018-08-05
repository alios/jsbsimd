{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSBSim.Types where

import Text.XML.HXT.Arrow.Edit
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.WriteDocument
import Data.JSBSim.Helper
import Control.Lens.At
import           Control.Lens.Getter
import           Control.Lens.TH
import           Control.Lens.Operators hiding ((*~))
import           Control.Lens.Iso
import           Control.Lens.Review
import           Control.Lens.Fold
import           Control.Lens.Prism
import           Data.Aeson
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Typeable                     (Typeable)
import           GHC.Generics                      (Generic)
import           Numeric.Units.Dimensional.Prelude as Dim
import           Numeric.Units.Dimensional.UnitNames (UnitName)
import qualified Prelude
import           Servant.API
import           Text.Read
import           Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.UnitNames.InterchangeNames (InterchangeName, HasInterchangeName(..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Read
import Control.Monad
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Pickle.Xml
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.Arrow.XmlState
import qualified Data.Bson as Bson
--import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Core ((>>>))
import Control.Arrow.ListArrow
import Text.XML.HXT.Arrow.XmlOptions
import Text.XML.HXT.DOM.ShowXml
import qualified Data.ByteString.Lazy as BL

newtype SimulatorInstanceId = SimulatorInstanceId Int
  deriving (Show, Eq, Ord, Typeable, Generic)

makePrisms ''SimulatorInstanceId
makeClassy ''SimulatorInstanceId

data SimulatorInstance = SimulatorInstance  {
  _simulatorId :: SimulatorInstanceId
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''SimulatorInstance
makePrisms ''SimulatorInstance

instance HasSimulatorInstanceId SimulatorInstance where
  simulatorInstanceId = simulatorId

data SimulatorInstanceCommand
  = SimulatorHalt
  | SimulatorRun
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON SimulatorInstanceCommand
instance FromJSON SimulatorInstanceCommand

instance ToJSON SimulatorInstanceId
instance FromJSON SimulatorInstanceId

instance ToJSON SimulatorInstance
instance FromJSON SimulatorInstance

instance FromHttpApiData SimulatorInstanceId where
  parseUrlPiece a =
    maybe (Left a) Right . fmap SimulatorInstanceId . readMaybe . T.unpack $ a

instance ToHttpApiData SimulatorInstanceId where
  toUrlPiece  = T.pack . show .  view _SimulatorInstanceId

--  parseUrlPiece a =
--    maybe (Left a) Right . fmap SimulatorInstanceId . readMaybe . T.unpack $ a





















data JsbSimEvent = JsbSimEvent {
  _eventName :: Maybe Text,
  _eventPersistent :: Bool,
  _eventContinuous :: Bool,
  _eventType :: Maybe Text,
  _eventDescription :: Maybe Text,
  _eventCondition :: Text,
  _eventNotify :: Vector Text,
  _eventSets :: HashMap Text (Text, Maybe Text)
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''JsbSimEvent
makePrisms ''JsbSimEvent


instance ToJSON JsbSimEvent where
 toEncoding = genericToEncoding aesonOpts
 toJSON = genericToJSON aesonOpts

instance FromJSON JsbSimEvent where
  parseJSON = genericParseJSON aesonOpts

-- | The "run" element has as its parent the "runscript" element.
--   The run element contains any and all "event" elements. Within the run element,
--   the start and end time must be specified, and the integration timestep
--   size (the inverse of the integration frequency).
--   Note that the start time will almost always be 0.0, and so the end time
--   merely specifies the duration of the run.
data JsbSimRunConfig t = JsbSimRunConfig {
  _runConfigStart      :: Time t,
  _runConfigEnd        :: Time t,
  _runConfigDT         :: Frequency t,
  _runConfigProperties :: HashMap Text (Maybe Text),
  _runConfigEvents     :: Vector JsbSimEvent
  } deriving (Eq, Show, Typeable, Generic)
makeClassy ''JsbSimRunConfig
makePrisms ''JsbSimRunConfig


instance (ToJSON t, FromJSON t, Fractional t) => ToJSON (JsbSimRunConfig t) where
 toEncoding = genericToEncoding aesonOpts
 toJSON = genericToJSON aesonOpts

instance (ToJSON t, FromJSON t, Fractional t) => FromJSON (JsbSimRunConfig t) where
  parseJSON = genericParseJSON aesonOpts


type JsbSimScriptName = Text

data JsbSimScript t = JsbSimScript {
  _scriptName        :: JsbSimScriptName,
  _scriptDescription :: Maybe Text,
  _scriptUseAircraft :: Text,
  _scriptUseAircraftInit :: Text,
  _scriptRun         :: JsbSimRunConfig t
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''JsbSimScript
makePrisms ''JsbSimScript

instance (ToJSON t, FromJSON t, Fractional t) => ToJSON (JsbSimScript t) where
 toEncoding = genericToEncoding aesonOpts
 toJSON = genericToJSON aesonOpts

instance (ToJSON t, FromJSON t, Fractional t) => FromJSON (JsbSimScript t) where
  parseJSON = genericParseJSON aesonOpts


instance HasJsbSimRunConfig (JsbSimScript t) t where
  jsbSimRunConfig = scriptRun


instance (Eq a, Show a, Read a, Fractional a) => XmlPickler (JsbSimRunConfig a) where
  xpickle = xpElem "run" . xpWrapIso (from _JsbSimRunConfig) $
            xp5Tuple xpStart xpEnd xpDT xpPs xpEs
    where
      xpStart =
        xpDefault (0 *~ second) . xpWrapPrism (_ReadQuantity second) .
        xpAttr "start" $ xpText
      xpEnd =
        xpWrapPrism (_ReadQuantity second) . xpAttr "end" $ xpText
      xpDT =
        xpWrapPrism (_ReadQuantity second . _TimeFrequency) . xpAttr "dt" $ xpText
      xpPs = xpWrap (HashMap.fromList, HashMap.toList) . xpList . xpElem "property" $ xpPair (xpT $ xpText) (xpOption . xpT $ xpAttr "value" xpText)
      xpEs = xpWrap (V.fromList, V.toList) $ xpList xpickle


instance XmlPickler JsbSimEvent where
  xpickle = xpElem "event" $
    xpWrapIso (from _JsbSimEvent) $
    xpWrap (f7t, t7t) $
    xp7Tuple xpName xpPers xpCont xpType xpDesc xpCond xpNotifySets
    where xpName = xpOption . xpT . xpAttr "name" $ xpText
          xpPers = xpDefault False $ xpAttr "persistent" xpBool
          xpCont = xpDefault False $ xpAttr "continuous" xpBool
          xpType = xpOption . xpT . xpAttr "type" $ xpText
          xpDesc = xpOption . xpT . xpElem "description" $ xpText
          xpCond = xpT . xpElem "condition" $ xpText
          xpNotifySets = xpBoth xpNotify xpEventSets
          xpNotify = xpWrap (V.fromList, V.toList) $
            xpElem "notify" $ xpList . xpT $
            xpElem "property" xpText
          xpEventSets = xpWrap (HashMap.fromList, HashMap.toList) $ xpList xpEventSet
          xpEventSet = xpElem "set" $ xpPair
            (xpT $ xpAttr "name" xpText) $ xpPair
              (xpT $ xpAttr "value" xpText)
              (xpOption . xpT $ xpAttr "type" xpText)
          f7t (a,b,c,d,e,f,(g, h)) = (a,b,c,d,e,f,g,h)
          t7t (a,b,c,d,e,f,g,h) = (a,b,c,d,e,f,(g, h))


instance (Eq a, Show a, Read a, Fractional a) => XmlPickler (JsbSimScript a) where
  xpickle =
    xpElem "runscript" .
    xpAddNSDecl "xsi" "http://www.w3.org/2001/XMLSchema-instance" .
    xpAddFixedAttr "xsi:noNamespaceSchemaLocation" "http://jsbsim.sf.net/JSBSimScript.xsd" .
    xpWrapIso (from _JsbSimScript) . xpWrap (f4t, t4f) $
      xp4Tuple xpName xpDesc xpUse xpRun
    where
      xpName = xpT . xpAttr "name" $ xpText
      xpDesc = xpOption . xpT . xpElem "description" $ xpText
      xpUse = xpElem "use" $ xpPair (xpAt "aircraft") (xpAt "initialize")
      xpAt n = xpT . xpAttr n $ xpText
      xpRun = xpickle
      f4t (a, b, (c, d), e) = (a, b, c, d, e)
      t4f (a, b, c, d, e) = (a, b, (c, d), e)










foo = samlToXML . head  <$> runX p
  where p =
          xunpickleDocument (xpickle :: PU (JsbSimScript Double))
          [ withValidate no
          , withTrace 1
          , withRemoveWS yes
          , withPreserveComment no
          ] "/Users/alios/src/jsbsim/scripts/Short_S23_4.xml"

cccc a = xshowBlob . pure $ pickleDoc xpickle a

qq = 34.2341 *~ knot

aaa = 92 *~ hertz

qqq :: Result (Quantity DVelocity Double)
qqq =
  let v = toJSON $ ((34.2341 :: Double) *~ knot)
  in fromJSON v

qqqq =case qqq of
  Success q -> q /~ knot
  Error e -> error e



b3 :: IO (Either String (JsbSimScript Double))
b3 = unpickleDoc' xpickle <$> b2

b2 = last . runLA (xreadDoc >>> removeAllWhiteSpace >>> removeAllComment) <$> readFile "/tmp/x.xml"
