{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.JSBSim.Script
  ( JsbSimScript, HasJsbSimScript(..)
  , JsbSimRunConfig, HasJsbSimRunConfig(..)
  , JsbSimEvent, HasJsbSimEvent(..)
  ) where


import           Control.Lens.At
import           Control.Lens.Fold
import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Lens.Operators                               hiding
                                                                       ((*~))
import           Control.Lens.Prism
import           Control.Lens.Review
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.Bson                                            as Bson
import qualified Data.ByteString.Lazy                                 as BL
import           Data.Conduit
import qualified Data.Conduit.List                                    as CL
import           Data.HashMap.Lazy                                    (HashMap)
import qualified Data.HashMap.Lazy                                    as HashMap
import           Data.JSBSim.Helper
import           Data.JSBSim.XML
import           Data.JSBSim.XML
import           Data.Maybe
import           Data.Text                                            (Text)
import           Data.Text                                            (Text,
                                                                       unpack)
import qualified Data.Text                                            as T
import           Data.Typeable                                        (Typeable)
import           Data.Vector                                          (Vector)
import qualified Data.Vector                                          as V
import           Data.XML.Types
import qualified Data.XML.Types                                       as XML
import           GHC.Generics                                         (Generic)
import           Numeric.Units.Dimensional.NonSI
import           Numeric.Units.Dimensional.Prelude                    as Dim
import           Numeric.Units.Dimensional.UnitNames                  (UnitName)
import           Numeric.Units.Dimensional.UnitNames.InterchangeNames (HasInterchangeName (..),
                                                                       InterchangeName)
import qualified Prelude
import           Servant.API
import           Text.Read
import           Text.Read
import           Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render                               as XR


data JsbSimSetterAction t
  = FG_RAMP (Maybe (Time t))
  | FG_STEP (Time t)
  | FG_EXP (Time t)
  deriving (Eq, Show, Typeable, Generic)


data JsbSimSetterType
  = FG_VALUE
  | FG_DELTA
  deriving (Eq, Show, Enum, Typeable, Generic)

data JsbSimSetter t = JsbSimSetter {
  _setterName :: Text,
  _setterType :: JsbSimSetterType,
  _setterAction :: JsbSimSetterAction t,
  _setterValue :: Maybe Text
  } deriving (Eq, Show, Typeable, Generic)

parseJsbSimSetterAction ::
  (Show t, Read t, Fractional t) => AttrParser (JsbSimSetterAction t)
parseJsbSimSetterAction = do
  r <- fromMaybe "FG_RAMP" <$> attr "action"
  case r of
    "FG_RAMP" -> FG_RAMP <$> pT
    "FG_STEP" -> FG_STEP <$> force "unable to read tc for FG_STEP" pT
    "FG_EXP" -> FG_EXP <$> force "unable to read tc for FG_EXP" pT
  where pT = (maybe Nothing (preview (_ReadQuantity second) . T.unpack)) <$> attr "tc"

parseJsbSimSetterType :: AttrParser JsbSimSetterType
parseJsbSimSetterType = f <$> attr "type"
  where f (Just "FG_VALUE") = FG_VALUE
        f (Just "FG_DELTA") = FG_DELTA
        f Nothing = FG_VALUE
        f (Just a) = undefined --fail $ "invalid setter type " ++ T.unpack a

instance (Show t, Read t, Fractional t) => FromXML (JsbSimSetter t) where
  fromXML =  tag' "set" spA pure
    where
      spA = JsbSimSetter
        <$> requireAttr "name"
        <*> parseJsbSimSetterType
        <*> parseJsbSimSetterAction
        <*> attr "value"

instance ToXML (JsbSimSetter t) where

data JsbSimEvent t = JsbSimEvent {
  _eventName        :: Maybe Text,
  _eventPersistent  :: Bool,
  _eventContinuous  :: Bool,
  _eventType        :: Maybe Text,
  _eventDescription :: Maybe Text,
  _eventCondition   :: Text,
  _eventDelay       :: Maybe (Time t),
  _eventNotify      :: Vector (Text, Maybe Text),
  _eventSets        :: Vector (JsbSimSetter t)
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''JsbSimEvent
makePrisms ''JsbSimEvent

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
  _runConfigEvents     :: Vector (JsbSimEvent t)
  } deriving (Eq, Show, Typeable, Generic)
makeClassy ''JsbSimRunConfig
makePrisms ''JsbSimRunConfig


data JsbSimScript t = JsbSimScript {
  _scriptName         :: Text,
  _scriptDescription  :: Maybe Text,
  _scriptAircraft     :: Text,
  _scriptAircraftInit :: Text,
  _scriptRun          :: JsbSimRunConfig t
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''JsbSimScript
makePrisms ''JsbSimScript

instance ToJSON JsbSimSetterType where
 toEncoding = genericToEncoding aesonOpts
 toJSON = genericToJSON aesonOpts

instance (Fractional t, ToJSON t, FromJSON t) => ToJSON (JsbSimSetterAction t) where
 toEncoding = genericToEncoding aesonOpts
 toJSON = genericToJSON aesonOpts


instance (Fractional t, ToJSON t, FromJSON t) => ToJSON (JsbSimSetter t) where
 toEncoding = genericToEncoding aesonOpts
 toJSON = genericToJSON aesonOpts

instance  (Fractional t, ToJSON t, FromJSON t) => ToJSON (JsbSimEvent t) where
 toEncoding = genericToEncoding aesonOpts
 toJSON = genericToJSON aesonOpts


instance FromJSON JsbSimSetterType where
  parseJSON = genericParseJSON aesonOpts

instance  (Fractional t, ToJSON t, FromJSON t) => FromJSON (JsbSimSetterAction t) where
  parseJSON = genericParseJSON aesonOpts

instance  (Fractional t, ToJSON t, FromJSON t) => FromJSON (JsbSimSetter t) where
  parseJSON = genericParseJSON aesonOpts

instance  (Fractional t, ToJSON t, FromJSON t) => FromJSON (JsbSimEvent t) where
  parseJSON = genericParseJSON aesonOpts


instance (Show t, Read t, Fractional t, Eq t) => FromXML (JsbSimEvent t) where
  fromXML = tag' "event" aP $ \(n, p, c, t) -> do
    cs <- parseJsbSimEventChildren
    JsbSimEvent n p c t (eventCDesc cs) <$> eventCCond cs <*> pure (eventCDelay cs)
      <*> pure (eventCNotify cs) <*> pure (eventCSets cs)
      where aP = (,,,)
              <$> attr "name"
              <*> (parseXmlBool <$> attr "persistent")
              <*> (parseXmlBool <$> attr "continuous")
              <*> attr "type"

instance (Show t, Read t, Fractional t, Eq t) => ToXML (JsbSimEvent t) where
  toXML e = XR.tag "event" as $ do

    maybe (pure ()) (XR.tag "description" mempty . XR.content) $
      e ^. eventDescription

    XR.tag "condition" mempty . XR.content $ e ^. eventCondition

    maybe (pure ()) (XR.tag "delay" mempty . XR.content . T.pack . review (_ReadQuantity second) ) $
      e ^. eventDelay

    XR.tag "notify" mempty $
      mapM_ (\(n, c) -> XR.tag "property" (XR.optionalAttr "caption" c) .
                        XR.content $ n)  (e ^. eventNotify)
    if e ^. eventSets == mempty then pure () else
       sequence_ . fmap toXML $ e ^. eventSets

    where as = mconcat
            [ XR.optionalAttr "name" $ e ^. eventName
            , XR.optionalAttr "persistent" . renderXmlBool $ e ^. eventPersistent
            , XR.optionalAttr "continuous" . renderXmlBool $ e ^. eventContinuous
            , XR.optionalAttr "type" $ e ^. eventType
            ]
          toS (k, (v, t, a, tc)) = flip (XR.tag "set") (pure ()) . mconcat $
            [XR.attr "name" k, XR.attr "value" v, XR.optionalAttr "type" t, XR.optionalAttr "action" a, XR.optionalAttr "tc" tc]

instance (ToJSON t, FromJSON t, Fractional t) => ToJSON (JsbSimRunConfig t) where
 toEncoding = genericToEncoding aesonOpts
 toJSON = genericToJSON aesonOpts

instance (ToJSON t, FromJSON t, Fractional t) => FromJSON (JsbSimRunConfig t) where
  parseJSON = genericParseJSON aesonOpts


instance  (Show a, Read a, Fractional a, Eq a) => FromXML (JsbSimRunConfig a) where
  fromXML = tag' "run" aP cP
    where
      aP = (,,)
        <$> (pT "start" >>= maybe (pure $ 0 *~ second) pure)
        <*> force "parseJsbSimRunConfig: unable to parse end time" (pT  "end")
        <*> force "parseJsbSimRunConfig: unable to parse dt" (pF "dt")
      cP (st, et, dt) =
        uncurry (JsbSimRunConfig st et dt) <$> parseJsbSimRunConfigChildren
      pF n =
        ((preview (_ReadQuantity second . _TimeFrequency) . T.unpack) =<<)
        <$> attr n
      pT n = ((preview (_ReadQuantity second) . T.unpack) =<<)
        <$> attr n


instance (Eq a, Show a, Read a, Fractional a) => ToXML (JsbSimRunConfig a) where
  toXML c = XR.tag "run" as $ do
    mapM_  toP . HashMap.toList $ c ^. runConfigProperties
    mapM_ toXML $ c ^. runConfigEvents
    where as = mconcat
            [ XR.attr "start" . T.pack $
              c ^. runConfigStart . re (_ReadQuantity second)
            , XR.attr "end" . T.pack $
              c ^. runConfigEnd . re (_ReadQuantity second)
            , XR.attr "dt" . T.pack $
              c ^. runConfigDT . re (_ReadQuantity second . _TimeFrequency)
            ]
          toP (k, v) = XR.tag "property" (XR.optionalAttr "value" v) (XR.content k)


instance HasJsbSimRunConfig (JsbSimScript t) t where
  jsbSimRunConfig = scriptRun

instance (ToJSON t, FromJSON t, Fractional t) => ToJSON (JsbSimScript t) where
 toEncoding = genericToEncoding aesonOpts
 toJSON = genericToJSON aesonOpts

instance (ToJSON t, FromJSON t, Fractional t) => FromJSON (JsbSimScript t) where
  parseJSON = genericParseJSON aesonOpts


instance (Show a, Read a, Fractional a, Eq a) => FromXML (JsbSimScript a) where
  fromXML =
    tag' "runscript"  aP $ \n -> do
      cs <- parseJsbSimScriptChildren
      (an, ai) <- scriptCAircraft cs
      JsbSimScript n (scriptCDesc cs) an ai <$> scriptCRun cs
      where aP = do
              r <- requireAttr "name"
              ignoreAttrs
              return r

instance (Eq a,Show a, Read a, Fractional a) => ToXML (JsbSimScript a) where
  toXML s = do
    yieldXSL "/JSBSimScript.xsl"
    XR.tag "runscript" as $ do
      maybe (pure ()) (XR.tag "description" mempty . XR.content) $
        s ^. scriptDescription
      XR.tag "use" aas (pure ())
      toXML $ s ^. jsbSimRunConfig
      where as = mconcat
              [ mkXSDAttrs "/JSBSimScript.xsd"
              , XR.attr "name" $ s ^. scriptName
              ]
            aas = mconcat
              [ XR.attr "aircraft" $ s ^. scriptAircraft
              , XR.attr "initialize" $ s ^. scriptAircraftInit
              ]




parseJsbSimEventChildren ::
  (Show t, Read t, Fractional t, Eq t, MonadParseXML m) =>
  ConduitT XML.Event Void m [EventChild t]
parseJsbSimEventChildren = many parseJsbSimEventChild
  where
    parseJsbSimEventChild = do
      r <- choose
        [ fmap (EventDesc . pure) <$> tagNoAttr "description" content
        , fmap EventCond <$> tagNoAttr "condition" content
        , fmap (EventNotify . V.fromList) <$> tagNoAttr "notify" (many pP)
        , fmap EventSets <$> parseSets
        , fmap EventDelay <$> delayP
        ]
      return r
    pP = tag' "property" (attr "caption") (\c -> (,) <$> content <*> pure c)
    parseSets = do
      ss <- many fromXML
      return $ if ss == mempty then Nothing else Just (V.fromList ss)
    delayP = (maybe Nothing (preview (_ReadQuantity second) . T.unpack)) <$>
      tagNoAttr "delay" content

parseJsbSimRunConfigChildren ::
  (Show t, Read t, Fractional t, Eq t, MonadParseXML m) =>
  ConduitT XML.Event Void m (HashMap Text (Maybe Text), Vector (JsbSimEvent t))
parseJsbSimRunConfigChildren = do
  a <- parseOneChild
  b <- parseOneChild
  case (a,b) of
    (Nothing, Nothing)                -> pure (mempty, mempty)
    (Just (Left a'), Nothing)         -> pure (a', mempty)
    (Just (Right b'), Nothing)        -> pure (mempty, b')
    (Nothing, Just (Left a'))         -> pure (a', mempty)
    (Nothing, Just (Right b'))        -> pure (mempty, b')
    (Just (Left a'), Just (Right b')) -> pure (a', b')
    (Just (Right b'), Just (Left a')) -> pure (a', b')
  where
    pP = tag' "property" (attr "value") (\v -> (,) <$> content <*> pure v)
    parseOneChild = choose
      [ (\m -> if m == mempty then Nothing else
                 pure . Left . HashMap.fromList $ m) <$> many pP
      , (\v -> if v == mempty then Nothing else
                 pure . Right . V.fromList $ v) <$> many fromXML
      ]

parseJsbSimScriptChildren :: (MonadParseXML m, Show t, Read t, Fractional t, Eq t) =>
  ConduitT XML.Event Void m [ScriptChild t]
parseJsbSimScriptChildren = many $ choose
  [ mapChild (ScriptDesc . pure ) $ tagNoAttr "description" content
  , mapChild ScriptAircraft $ tag' "use" ((,) <$> requireAttr "aircraft" <*> requireAttr "initialize") pure
  , mapChild ScriptRun fromXML
  ]
  where mapChild c = fmap (maybe Nothing (pure . c))


data EventChild t
  = EventDesc (Maybe Text)
  | EventCond Text
  | EventNotify (Vector (Text, Maybe Text))
  | EventSets (Vector (JsbSimSetter t))
  | EventDelay (Time t)
  deriving (Eq, Show)

data ScriptChild t
  = ScriptDesc (Maybe Text)
  | ScriptAircraft (Text, Text)
  | ScriptRun (JsbSimRunConfig t)


eventCDesc :: [EventChild t] -> Maybe Text
eventCDesc = join . listToMaybe . f
  where
    f []                 = []
    f (EventDesc a : as) = a : f as
    f (_ : as)           =  f as

eventCCond :: MonadParseXML m => [EventChild t] -> m Text
eventCCond = force "expecting condition tag" . pure . listToMaybe . f
  where
    f []                 = []
    f (EventCond a : as) = a : f as
    f (_ : as)           =  f as

eventCDelay :: [EventChild t] -> Maybe (Time t)
eventCDelay =  listToMaybe . f
  where
    f []                 = []
    f (EventDelay a : as) = a : f as
    f (_ : as)           =  f as

eventCNotify :: [EventChild t] -> Vector (Text, Maybe Text)
eventCNotify = fromMaybe mempty . listToMaybe . eventCNotify'
  where
    eventCNotify' []                   = []
    eventCNotify' (EventNotify a : as) = a : eventCNotify' as
    eventCNotify' (_ : as)             =  eventCNotify' as

eventCSets :: [EventChild t] -> Vector (JsbSimSetter t)
eventCSets = fromMaybe mempty . listToMaybe . eventCSets'
  where
    eventCSets' []                 = []
    eventCSets' (EventSets a : as) = a : eventCSets' as
    eventCSets' (_ : as)           =  eventCSets' as



scriptCDesc :: [ScriptChild t] -> Maybe Text
scriptCDesc = join . listToMaybe . f
  where f []                  = []
        f (ScriptDesc a : as) = a : f as
        f (_ : as)            = f as

scriptCAircraft :: (MonadParseXML m) => [ScriptChild t] -> m (Text, Text)
scriptCAircraft = force "unable to parse aircraft" . pure . listToMaybe . f
  where f []                      = []
        f (ScriptAircraft a : as) = a : f as
        f (_ : as)                = f as


scriptCRun :: (MonadParseXML m) => [ScriptChild t] -> m (JsbSimRunConfig t)
scriptCRun = force "unable to parse run config" . pure . listToMaybe . f
  where f []                 = []
        f (ScriptRun a : as) = a : f as
        f (_ : as)           = f as
