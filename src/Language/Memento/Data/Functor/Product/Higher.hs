{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.Memento.Data.Functor.Product.Higher (
  (:**:) (..),
  HUnit (..),
  HExtractive (..),
  HProduct,
  HConstructive (..),
  hSingleton,
) where

import           Data.Kind                            (Type)
import           Language.Memento.Data.Functor.Higher (HFunctor (hmap))

data (h1 :**: h2) (f :: k -> Type) a
  = (h1 f a) :**: (h2 f a)

deriving instance (Show (h1 f a), Show (h2 f a)) => Show ((h1 :**: h2) f a)
deriving instance (Eq (h1 f a), Eq (h2 f a)) => Eq ((h1 :**: h2) f a)
deriving instance (Ord (h1 f a), Ord (h2 f a)) => Ord ((h1 :**: h2) f a)

data HUnit (f :: k -> Type) a = HUnit

deriving instance Show (HUnit f a)
deriving instance Eq (HUnit f a)
deriving instance Ord (HUnit f a)

type family HProduct hs where
  HProduct '[] = HUnit
  HProduct (h ': hs) = h :**: HProduct hs

instance HFunctor HUnit where
  hmap _ HUnit = HUnit

instance (HFunctor h1, HFunctor h2) => HFunctor (h1 :**: h2) where
  hmap f (h1 :**: h2) = hmap f h1 :**: hmap f h2

-- | Extract a component from a product type.
class HExtractive h1 h2 where
  hExtract :: forall (f :: Type -> Type) a. h2 f a -> h1 f a

instance {-# OVERLAPPING #-} HExtractive h1 (h1 :**: h2) where
  hExtract :: (h1 :**: h2) f a -> h1 f a
  hExtract (h1 :**: _) = h1

instance {-# OVERLAPPABLE #-} (HExtractive h1 h2) => HExtractive h1 (h3 :**: h2) where
  hExtract :: (h3 :**: h2) f a -> h1 f a
  hExtract (_ :**: h2) = hExtract h2

{-
Helpers for constructing product types.
-}

class HConstructive (h1 :: (k -> Type) -> k -> Type) h2 h3 where
  hConstruct :: forall (f :: k -> Type) a. h1 f a -> h2 f a -> h3 f a

instance {-# OVERLAPPING #-} HConstructive h1 h2 (h1 :**: h2) where
  hConstruct :: h1 f a -> h2 f a -> (h1 :**: h2) f a
  hConstruct h1 h2 = h1 :**: h2

instance {-# OVERLAPPABLE #-} (HConstructive h1 h2 h3) => HConstructive h1 (h4 :**: h2) (h4 :**: h3) where
  hConstruct :: h1 f a -> (h4 :**: h2) f a -> (h4 :**: h3) f a
  hConstruct h1 (h4 :**: h2) = h4 :**: hConstruct h1 h2

hSingleton :: h f a -> HProduct '[h] f a
hSingleton h = h :**: HUnit
