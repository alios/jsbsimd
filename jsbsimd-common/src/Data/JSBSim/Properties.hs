{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Data.JSBSim.Properties where

import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Lens.Lens
import           Control.Lens.Review
import           Control.Lens.TH
import           Control.Lens.Tuple
import           Data.HList.CommonMain
import           Data.Proxy
import           Data.Text
import           Data.Typeable                                        (Typeable)
import           GHC.TypeLits
import           Numeric.Units.Dimensional.Prelude                    hiding
                                                                       (_1, _2)
import qualified Numeric.Units.Dimensional.UnitNames.InterchangeNames as Units
import qualified Prelude                                              ()

type PropertyPath = String

class HasPropertyPath a where
  propertyPath :: Getter a PropertyPath



--
-- Properties
--

data PropertyType
  = NumericProperty
  | BoolProperty
  | StringProperty
  deriving (Eq, Typeable)



data Property (t :: PropertyType) (d :: Dimension) a where
  MkNumericProperty :: (KnownDimension d, Fractional a) =>
    PropertyPath -> Unit m d a -> Property 'NumericProperty d a
  MkBoolProperty :: PropertyPath -> Property 'BoolProperty Any Bool
  MkStringProperty :: PropertyPath -> Property StringProperty Any Text
  deriving (Typeable)

makePrisms ''Property
makeClassy ''Property

instance Show (Property t d a) where
  show (MkNumericProperty p u) = show (p, name u)
  show (MkBoolProperty p)      = show p
  show (MkStringProperty p)    = show p


instance HasPropertyPath (Property t d a) where
  propertyPath = to toP
    where
      toP :: Property t d a -> PropertyPath
      toP (MkNumericProperty p _) = p
      toP (MkBoolProperty p)      = p
      toP (MkStringProperty p)    = p


--
-- Property Values
--


class HasPropertyValueType (t :: PropertyType) a where
  type PropertyValueType t (d :: Dimension) a :: *
  _PropertyValueBaseType ::
    Iso' (PropertyValue t d a) (Property t d a, a)

newtype PropertyValue t d a =
  PropertyValue (Property t d a, PropertyValueType t d a)

makePrisms ''PropertyValue


instance HasPropertyValueType 'NumericProperty a where
  type PropertyValueType 'NumericProperty d a = Quantity d a
  _PropertyValueBaseType = iso t f
    where
      f :: (Property 'NumericProperty d a, a) -> PropertyValue 'NumericProperty d a
      f (p@(MkNumericProperty _ u), a) = _PropertyValue # (p, a *~ u)
      t :: PropertyValue 'NumericProperty d a -> (Property 'NumericProperty d a, a)
      t (PropertyValue (p@(MkNumericProperty _ u), q))  = (p, q /~ u)

instance HasPropertyValueType 'StringProperty Text where
  type PropertyValueType 'StringProperty d Text = Text
  _PropertyValueBaseType = _PropertyValue

instance HasPropertyValueType 'BoolProperty Bool where
  type PropertyValueType 'BoolProperty d Bool = Bool
  _PropertyValueBaseType = _PropertyValue







propertyValueProperty :: Lens' (PropertyValue t d a) (Property t d a)
propertyValueProperty = _PropertyValue . _1

propertyValueValue :: Lens' (PropertyValue t d a) (PropertyValueType t d a)
propertyValueValue = _PropertyValue . _2


instance HasPropertyPath (PropertyValue t d a) where
  propertyPath = propertyValueProperty . propertyPath

instance (KnownDimension d, Real a, Show a) =>
  Show (PropertyValue 'NumericProperty d a) where
  show a = show $ (a ^. propertyValueProperty, a ^. propertyValueValue)

instance Show (PropertyValue 'BoolProperty d Bool) where
  show a = show $ (a ^. propertyValueProperty, a ^. propertyValueValue)

instance Show (PropertyValue 'StringProperty d Text) where
  show a = show $ (a ^. propertyValueProperty, a ^. propertyValueValue)








{-
newtype PropertyList ts ds as
  PropertyList :: (HList ((Property t d a) : '[Property ts ds as])) ->
    PropertyList (t : ts) (d : ds) (a : as)
-}

pa = MkNumericProperty "/tree/velocity" (kilo meter / hour)
pb = MkBoolProperty "/tree/isNice"

pva = _PropertyValueBaseType # (pa, 152.3)
pvb = _PropertyValueBaseType # (pb, True)

foo = HCons pa (HCons pb HNil)

bar = HCons pva (HCons pvb HNil)


-- bbaz = _PropertyList # foo






{-


--
-- Lists
--

class HasPropertyList (p :: PropertyType -> Dimension -> * -> *) where
  data PropertyList p :: [PropertyType] -> [Dimension] -> [*] -> *

  emptyPropertyList :: PropertyList p '[] '[] '[]
  (<::>) :: p t d a -> PropertyList p ts ds as ->
    PropertyList p (t : ts) (d : ds) (a :as)

  propertyListUncons :: PropertyList p (t : ts) (d : ds) (a : as) ->
    (p t d a, PropertyList p ts ds as)







purePropertyList :: HasPropertyList p => p t d a -> PropertyList p '[t] '[d] '[a]
purePropertyList a = a <::> emptyPropertyList

instance HasPropertyList Property where
  data PropertyList Property ts ds as where
    EmptyPropertyList :: PropertyList Property '[] '[] '[]
    ConsPropertyList ::
      Property t d a -> PropertyList Property ts ds as ->
      PropertyList Property (t:ts) (d : ds) (a : as)
  emptyPropertyList = EmptyPropertyList
  (<::>) = ConsPropertyList
  propertyListUncons (ConsPropertyList p ps) = (p, ps)

instance HasPropertyList PropertyValue where
  data PropertyList PropertyValue ts ds as where
    EmptyPropertyValueList :: PropertyList PropertyValue '[] '[] '[]
    ConsPropertyValueList ::
      PropertyValue t d a -> PropertyList PropertyValue ts ds as ->
      PropertyList PropertyValue (t:ts) (d : ds) (a : as)
  emptyPropertyList = EmptyPropertyValueList
  (<::>) = ConsPropertyValueList
  propertyListUncons (ConsPropertyValueList p ps) = (p, ps)



data PropertyFunctionList p ts ds as bs where
  EmptyPropertyFunctionList :: PropertyFunctionList p '[] '[] '[] '[]
  ConsPropertyFunctionList :: HasPropertyList p =>
    (p t d a -> b) -> PropertyFunctionList p ts ds as bs ->
    PropertyFunctionList p (t : ts) (d : ds) (a : as) (b : bs)

data HList as where
  HListEmpty :: HList '[]
  HListCons :: a -> HList as -> HList (a : as)

mapFunctionList ::
  PropertyFunctionList p ts ds as bs -> PropertyList p ts ds as -> HList bs
mapFunctionList (EmptyPropertyFunctionList) _ = HListEmpty
mapFunctionList (ConsPropertyFunctionList f fs) vs =
  let (p, ps) = propertyListUncons vs
  in HListCons (f p) (mapFunctionList fs ps)


propertyListMap :: p t d a ->

propertyListMapHead :: HasPropertyList p => (p t d a -> b) ->
  PropertyList p (t : ts) (d : ds) (a : as) ->
  (b, PropertyList p ts ds as)
propertyListMapHead f pl =
  let (p, ps) = propertyListUncons pl
  in (f p, ps)

{-
propertyListFoldMap :: HasPropertyList p =>
  bs -> (b -> bs -> bs) -> (p t d a -> b) ->
  PropertyList p ts ds as -> bs
propertyListFoldMap z g f pl
  | pl == emptyPropertyList = z
  | otherwise =
      let (b, ps) = propertyListMapHead f pl
      in g b $ propertyListFoldMap g f ps
-}



ta = MkNumericProperty "/v" (kilo meter / hour)
tb = MkStringProperty "/s"

tl = tb <::> (ta <::> emptyPropertyList)

tav = (_NumericPropertyValue # (ta, 34))
tbv = (_PropertyValue # (tb, "foo bar"))
pl =  tav <::> emptyPropertyList

data Foo = Foo (Velocity Double) String String Int

fff = uncurry Foo


--xx = Foo _






{-

{-
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
-}

{-
data PropertyList (ds :: [ t :: k ]) a where
  PropertyListEmpty :: PropertyList '[]
  PropertyListCons :: IsProperty p' d' a' =>
    Property p' d' a' -> PropertyList ds' -> PropertyList ( (d', a') : ds') a
-}

--instance IsPropertyPath 'TypedPropertyPath where
--  data instance PropertyPathKind 'TypedPropertyPath where
--    TypedProperty :: (KnownSymbol p, IsProperty p d a) => Proxy p -> Property p d a ->
--      PropertyPathKind 'TypedPropertyPath


class (Eq (PropertyValueType d a)) => IsProperty p (d :: kd) a where
  data family Property p d a :: *
  type family PropertyValueType d a :: *

  _PropertyValue :: Iso' (PropertyValue p d a) (Property p d a, a)


class HasPropertyPath a where
  propertyPath :: Getter a PropertyPath

newtype PropertyValue p d a =
  PropertyValue (Property p d a, PropertyValueType d a)
  deriving (Typeable)

data NamedProperty d a where
  NamedProperty ::
    IsProperty p d a => PropertyPath -> Property p d a -> NamedProperty d a

data PropertyList (ds :: [d]) as where
  EmptyPropertyList :: PropertyList '[] '[]
  ConsPropertyList ::
    NamedProperty d a -> PropertyList ds as -> PropertyList (d : ds) (a : as)





fromTypedProperty :: (IsProperty p d a, KnownSymbol p) =>
  Property p d a -> NamedProperty d a
fromTypedProperty p = NamedProperty (p ^. propertyPath) p

fromNamedProperty :: IsProperty p d a =>
  PropertyPath -> Property p d a -> NamedProperty d a
fromNamedProperty n p = NamedProperty n p

instance (KnownDimension d, Fractional a, Eq a) =>
  IsProperty p (d :: Dimension) (a :: *) where
  data Property p d a
    = forall m . NumericProperty (Unit m d a)

  type PropertyValueType d a = Quantity d a

  _PropertyValue = iso f t
    where f (PropertyValue (p@(NumericProperty u), a)) = (p, a /~ u)
          t (p@(NumericProperty u), a) = PropertyValue (p, a *~ u)

instance (IsProperty p d a, KnownSymbol p) => HasPropertyPath (Property p d a) where
  propertyPath =
    to $ \p -> symbolVal $ proxy p
    where
      proxy :: Property p d a -> Proxy p
      proxy = const Proxy

instance IsProperty p () Text where
  data Property p () Text = TextProperty
    deriving (Eq)
  type PropertyValueType () Text = Text
  _PropertyValue = iso (\(PropertyValue v) -> v) PropertyValue


instance IsProperty p () Bool where
  data Property p () Bool = BoolProperty
    deriving (Eq)
  type PropertyValueType () Bool = Bool
  _PropertyValue = iso (\(PropertyValue v) -> v) PropertyValue


instance HasPropertyPath (NamedProperty d a) where
  propertyPath =
    to $ \(NamedProperty p _) -> p


{-
instance HasPropertyPath (Property 'TypedPropertyPath d a) where
  propertyPath = to $ undefined -- symbolVal . pProxy
    where pProxy :: Property p d a -> Proxy p
          pProxy = const Proxy

instance (IsProperty 'TypedPropertyPath d a) =>
  HasPropertyPath (PropertyValue 'TypedPropertyPath d a) where
  propertyPath = _PropertyValue . _1 . propertyPath
-}





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


fooo = (ConsPropertyList a EmptyPropertyList)
  where a = fromNamedProperty "/foo" (NumericProperty meter)
        b = fromNamedProperty "/bar" TextProperty
-}
-}
