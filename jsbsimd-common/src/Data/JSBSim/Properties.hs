{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
--{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}

module Data.JSBSim.Properties where

import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Lens.Review
import           Control.Lens.Tuple
import           Data.Proxy
import           Data.Text
import           Data.Typeable                                        (Typeable)
import           GHC.TypeLits
import           Numeric.Units.Dimensional.Prelude                    hiding
                                                                       (_1, _2)
import qualified Numeric.Units.Dimensional.UnitNames.InterchangeNames as Units
import qualified Prelude                                              ()


type PropertyPath = String


data PropertyPathType
  = NamedPropertyPath
  | TypedPropertyPath
  deriving (Eq, Show)


class IsPropertyPath p where
  data family PropertyPathKind (p :: PropertyPathType) :: *
  toPropertyPath :: Getter (Property p d a) PropertyPath

instance IsPropertyPath 'NamedPropertyPath where
  data instance PropertyPathKind 'NamedPropertyPath where
     NamedProperty :: IsProperty p d a => PropertyPath -> Property p d a ->
      PropertyPathKind 'NamedPropertyPath



instance IsPropertyPath 'TypedPropertyPath where
  data instance PropertyPathKind 'TypedPropertyPath where
    TypedProperty :: (KnownSymbol p, IsProperty p d a) => Proxy p => Property p d a ->
      PropertyPathKind 'TypedPropertyPath


--type instance PropertyPathKind 'TypedPropertyPath = Symbol




class (Eq (PropertyValueType d a), IsPropertyPath p) => IsProperty p (d :: kd) a where
  data family Property p d a :: *
  type family PropertyValueType d a :: *

  _PropertyValue :: Iso' (PropertyValue p d a) (Property p d a , a)


class HasPropertyPath a where
  propertyPath :: Getter a PropertyPath

newtype PropertyValue p d a =
  PropertyValue (Property p d a, PropertyValueType d a)
  deriving (Typeable)


instance (IsPropertyPath p, KnownDimension d, Fractional a, Eq a) =>
  IsProperty p (d :: Dimension) (a :: *) where
  data Property p d a
    = forall m . NumericProperty (Unit m d a)

  type PropertyValueType d a = Quantity d a

  _PropertyValue = iso f t
    where f (PropertyValue (p@(NumericProperty u), a)) = (p, a /~ u)
          t (p@(NumericProperty u), a) = PropertyValue (p, a *~ u)

instance IsPropertyPath p => IsProperty p () Text where
  data Property p () Text = TextProperty
    deriving (Eq)
  type PropertyValueType () Text = Text
  _PropertyValue = iso (\(PropertyValue v) -> v) PropertyValue


instance IsPropertyPath p => IsProperty p () Bool where
  data Property p () Bool = BoolProperty
    deriving (Eq)
  type PropertyValueType () Bool = Bool
  _PropertyValue = iso (\(PropertyValue v) -> v) PropertyValue



instance HasPropertyPath (Property 'TypedPropertyPath d a) where
  propertyPath = to $ undefined -- symbolVal . pProxy
    where pProxy :: Property p d a -> Proxy p
          pProxy = const Proxy

instance (IsProperty 'TypedPropertyPath d a) =>
  HasPropertyPath (PropertyValue 'TypedPropertyPath d a) where
  propertyPath = _PropertyValue . _1 . propertyPath






{-
instance (KnownDimension d, Fractional a, Eq a) => IsProperty PropertyPath d a where
  data Property PropertyPath d a = forall m .
    NamedNumericProperty (PropertyPath, Unit m d a)
  type PropertyValueType PropertyPath d a = Quantity d a

instance IsProperty PropertyPath () Text where
  data Property PropertyPath () Text = NamedTextProperty PropertyPath
  type PropertyValueType PropertyPath () Text = Text


instance IsProperty PropertyPath () Bool where
  data Property PropertyPath () Bool = NamedBoolProperty PropertyPath
  type PropertyValueType PropertyPath () Bool = Bool
-}


{-
  _PropertyValue = iso  f t
    where
      f ::
        PropertyValue PropertyPath d a -> (Property PropertyPath d a, PropertyValueType PropertyPath d a)
      f (PropertyValue (p@(NamedNumericProperty (pp, u), a))) = (p, a /~ u)
      t ::
        (Property PropertyPath d a, a) -> PropertyValue PropertyPath d a
      t (p@(NamedNumericProperty (pp,u)), a) =
        undefined
-}


{-
instance (HasPropertyPath(Property p d a), Show a) => Show (Property p (d :: Dimension) a) where
  show p@(NumericProperty u) =
    show (view propertyPath p, Units.name . Units.interchangeName $ u)

instance (Show a) => Show (Property p () a) where
  show = view propertyPath
-}
{-
instance (Show (PropertyValueType p d a), KnownSymbol p) =>
  Show (PropertyValue p d a) where
  show (PropertyValue (p, v)) = show (view propertyPath p, show v)
-}
