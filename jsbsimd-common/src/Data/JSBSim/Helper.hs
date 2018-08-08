{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.JSBSim.Helper where

import           Conduit
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
import qualified Data.Conduit.List                 as CL
import           Data.Int
import           Data.Maybe                        (listToMaybe)
import           Data.Scientific
import           Data.String
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Vector.Generic.Lens
import           Data.XML.Types
import           Numeric.Units.Dimensional.Prelude as Dim
import qualified Prelude
import           Text.Read                         (readMaybe)
import qualified Text.XML.Stream.Render            as XR


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











parseXmlBool :: (Eq a, IsString a) => Maybe a -> Bool
parseXmlBool (Just "true") = True
parseXmlBool _             = False


renderXmlBool :: IsString a => Bool -> Maybe a
renderXmlBool True  = Just "true"
renderXmlBool False = Nothing

filterEmptyContent :: Monad m => ConduitT Event Event m ()
filterEmptyContent = CL.filter f
  where
    f (EventContent (ContentText a)) = not . T.null . T.strip $ a
    f _                              = True


mkXSDAttrs :: Text -> XR.Attributes
mkXSDAttrs s = mconcat
  [ XR.attr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
  , XR.attr "xsi:noNamespaceSchemaLocation" s
  ]


yieldXSL :: Monad m => Text -> ConduitT i Event m ()
yieldXSL a = yield e
  where e = EventInstruction (
          Instruction {
              instructionTarget = "xml-stylesheet",
              instructionData = mconcat
                [ "type=\"text/xsl\" href=\"", a, "\""]
              }
          )
