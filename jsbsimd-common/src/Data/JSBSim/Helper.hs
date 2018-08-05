{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.JSBSim.Helper where

import           Control.Lens.At
import           Control.Lens.Fold
import           Control.Lens.Getter
import           Control.Lens.Indexed
import           Control.Lens.Iso
import           Control.Lens.Operators            hiding ((*~))
import           Control.Lens.Prism
import           Control.Lens.Review
import           Data.Aeson
import qualified Data.Bson                         as Bson
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.ByteString.Lazy.Char8        as BSLC
import           Data.Int
import           Data.Maybe                        (listToMaybe)
import           Data.Scientific
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Vector.Generic.Lens
import           Numeric.Units.Dimensional.Prelude as Dim
import qualified Prelude
import           Text.Read                         (readMaybe)
import           Text.XML.HXT.Arrow.Pickle.Xml
import           Text.XML.HXT.Core                 ((<+>), (>>>))
import qualified Text.XML.HXT.Core                 as HXT

xpWrapIso :: Iso' a b -> PU a -> PU b
xpWrapIso i = xpWrap (view i, review i)

xpWrapPrism :: Prism' a b -> PU a -> PU b
xpWrapPrism i = xpWrapMaybe (preview i, review i)

_Quantity :: Fractional a => Unit m d a -> Iso' a (Quantity d a)
_Quantity u = iso (*~ u) (/~ u)

_SiQuantity :: (Fractional t, KnownDimension d) => Iso' t (Quantity d t)
_SiQuantity = _Quantity siUnit

_Json :: (FromJSON a, ToJSON a) => Prism' Value a
_Json = prism' toJSON f
  where f a = case fromJSON a of
          Success a -> pure a
          Error e   -> Nothing

_Text :: Iso' String Text
_Text = iso T.pack T.unpack

_ShowRead :: (Show a, Read a) => Prism' String a
_ShowRead = prism' show readMaybe


_ReadQuantity :: (Show a, Read a, Fractional a) =>
  Unit m d a -> Prism' String (Quantity d a)
_ReadQuantity u = _ShowRead .  _Quantity u

_TimeFrequency :: Fractional t => Iso' (Time t) (Frequency t)
_TimeFrequency = iso (\i -> (1 *~ one) / i) (\i -> (1 *~ one) / i)


xpT :: PU String -> PU Text
xpT = xpWrapIso (_Text)

xpEither xpa xpb = xpAlt c [xpWrap (Left, \(Left a) -> a) xpa, xpWrap (Right, \(Right a) -> a) xpb]
  where c (Left _)  = 0
        c (Right _) = 1

xpBoth :: PU a -> PU b -> PU (a, b)
xpBoth a b = xpWrapEither (f, t) $ xpPair (xpEither a b) (xpEither a b)
  where f (Left a, Right b)  = Right (a, b)
        f (Right b, Left a)  = Right (a, b)
        f (Left a, Left b)   = Left "expected a and b but got: a and a"
        f (Right a, Right b) = Left "expected a and b but got: b and b"
        t (a, b) = (Left a, Right b)

xpBool :: PU Bool
xpBool = xpWrapEither (f, t) xpText
  where
    f "true"  = Right True
    f "false" = Right False
    f a       = Left $ "expected true|false, got: " ++ show a
    t True  = "true"
    t False = "false"

aesonOpts :: Options
aesonOpts = defaultOptions {
  fieldLabelModifier = drop 1
  }

instance (KnownDimension d, Fractional a, ToJSON a, FromJSON a) =>
  ToJSON (Quantity d a) where
  toJSON = view (re _SiQuantity . re _Json)


instance (KnownDimension d, Fractional a, ToJSON a, FromJSON a) =>
  FromJSON (Quantity d a) where
  parseJSON = maybe (fail "unable to parse Quantity") pure .
    preview (_Json . _SiQuantity)


aesonbson :: Value -> Bson.Value
aesonbson (Data.Aeson.Object o) = Bson.Doc $ ifoldr
  (\k v b -> mappend b . pure . (Bson.:=) k . aesonbson $ v) mempty o
aesonbson (Data.Aeson.Array a)  =
  Bson.Array . review vector . fmap aesonbson $ a
aesonbson (Data.Aeson.String s) = Bson.String s
aesonbson (Data.Aeson.Number n) = case floatingOrInteger n of
  Left r  -> Bson.Float r
  Right i ->
    let i32 = fromInteger i :: Int32
        i64 = fromInteger i :: Int64
    in if i32 >= minBound && i32 <= minBound then Bson.val i32 else Bson.val i64
aesonbson (Data.Aeson.Bool b)   = Bson.Bool b
aesonbson (Data.Aeson.Null)     = Bson.Null

aesonbson' :: ToJSON t => t -> Bson.Value
aesonbson' = aesonbson . toJSON

bsonaeson :: Bson.Value -> Maybe Value
bsonaeson (Bson.Doc a) = fmap (Object . f1) . sequence . fmap f2 $ a
  where f1 = foldr (\(k,v) b -> b & at k .~ (pure v)) mempty
        f2 ((Bson.:=) k v) = ((,) <$> pure k <*> bsonaeson v)
bsonaeson (Bson.Array a)  =
  fmap (Array . view vector) . sequence . fmap bsonaeson $ a
bsonaeson (Bson.String a) = pure (toJSON a)
bsonaeson (Bson.Int32 a)  = pure (toJSON a)
bsonaeson (Bson.Int64 a)  = pure (toJSON a)
bsonaeson (Bson.Float a)  = pure (toJSON a)
bsonaeson (Bson.Bool a)   = pure (Bool a)
bsonaeson (Bson.Null)     = pure Null
bsonaeson _               = Nothing

_JsonBson :: Prism' Bson.Value Value
_JsonBson = prism' aesonbson bsonaeson

_JsonBson' :: (ToJSON a, FromJSON a) => Prism' Bson.Value a
_JsonBson' = _JsonBson . _Json



samlToDoc :: HXT.XmlPickler a => a -> HXT.XmlTree
samlToDoc = head . HXT.runLA (HXT.addXmlPi >>> addXmlStylesheet) . HXT.pickleDoc HXT.xpickle


addXmlStylesheet :: HXT.ArrowXml a => a HXT.XmlTree HXT.XmlTree
addXmlStylesheet = HXT.insertChildrenAt 1 (mkStylesheet <+> HXT.txt "\n")

mkStylesheet :: HXT.ArrowXml a => a HXT.XmlTree HXT.XmlTree
mkStylesheet =
  HXT.mkPi (HXT.mkName "xml-stylesheet") HXT.none >>>
  HXT.addAttr HXT.a_type "text/xsl" >>>
  HXT.addAttr "href" "/JSBSimScript.xsl"


docToXML :: HXT.XmlTree -> BSL.ByteString
docToXML = BSL.concat . HXT.runLA (HXT.xshowBlob HXT.getChildren)

samlToXML :: HXT.XmlPickler a => a -> BSL.ByteString
samlToXML = docToXML . samlToDoc

xmlToDoc :: BSL.ByteString -> Maybe HXT.XmlTree
xmlToDoc bs =
  let ar = HXT.xreadDoc >>> HXT.removeAllWhiteSpace >>> HXT.removeAllComment
      r = HXT.runLA ar . BSLC.unpack $  bs
  in if r /= mempty then pure (last r) else Nothing


docToSAML :: HXT.XmlPickler a => HXT.XmlTree -> Either String a
docToSAML = HXT.unpickleDoc' HXT.xpickle

xmlToSAML :: HXT.XmlPickler a => BSL.ByteString -> Either String a
xmlToSAML = maybe (Left "invalid XML") docToSAML . xmlToDoc
