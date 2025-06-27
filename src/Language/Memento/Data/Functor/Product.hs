{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Memento.Data.Functor.Product (
  (:*:) (..),
  UnitF (..),
  Extractive (..),
  Constructive (..),
  Product,
  singleton,
) where

import           Data.Kind                                   (Type)
import           Language.Memento.Data.NaturalTransformation (type (~>))

data (f1 :*: f2) a
  = f1 a :*: f2 a

deriving instance (Show (f1 a), Show (f2 a)) => Show ((f1 :*: f2) a)
deriving instance (Eq (f1 a), Eq (f2 a)) => Eq ((f1 :*: f2) a)
deriving instance (Ord (f1 a), Ord (f2 a)) => Ord ((f1 :*: f2) a)

data UnitF a = UnitF

deriving instance Show (UnitF a)
deriving instance Eq (UnitF a)
deriving instance Ord (UnitF a)

type family Product hs where
  Product '[] = UnitF
  Product (h ': hs) = h :*: Product hs

class Extractive (f1 :: Type -> Type) f2 where
  extract :: f2 ~> f1

instance {-# OVERLAPPING #-} Extractive f1 (f1 :*: f2) where
  extract (f1 :*: _) = f1

instance {-# OVERLAPPABLE #-} (Extractive f1 f2) => Extractive f1 (f3 :*: f2) where
  extract (_ :*: f2) = extract f2

instance Functor UnitF where
  fmap _ UnitF = UnitF

instance (Functor f1, Functor f2) => Functor (f1 :*: f2) where
  fmap f (f1 :*: f2) = fmap f f1 :*: fmap f f2

class Constructive (f1 :: Type -> Type) f2 f3 where
  construct :: forall a. f1 a -> f2 a -> f3 a

instance {-# OVERLAPPING #-} Constructive f1 f2 (f1 :*: f2) where
  construct f1 f2 = f1 :*: f2

instance {-# OVERLAPPABLE #-} (Constructive f1 f2 f3) => Constructive f1 (f4 :*: f2) (f4 :*: f3) where
  construct f1 (f4 :*: f2) = f4 :*: construct f1 f2

singleton :: f a -> Product '[f] a
singleton f = f :*: UnitF
