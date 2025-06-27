{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Data.Functor.Coproduct.Higher (
  (:+:) (..),
  HVoid,
  Injective (..),
  IsVoidIn (..),
  SafeProjective (..),
  HCoproduct,
) where

import Data.Kind (Type)
import Language.Memento.Data.Functor.Higher (HFunctor (hmap))
import Language.Memento.Data.NaturalTransformation (type (~>))

data (h1 :+: h2) (f :: k -> Type) a
  = HInjL (h1 f a)
  | HInjR (h2 f a)

deriving instance (Show (h1 f a), Show (h2 f a)) => Show ((h1 :+: h2) f a)
deriving instance (Eq (h1 f a), Eq (h2 f a)) => Eq ((h1 :+: h2) f a)
deriving instance (Ord (h1 f a), Ord (h2 f a)) => Ord ((h1 :+: h2) f a)

data HVoid (f :: k -> Type) a

deriving instance Show (HVoid f a)
deriving instance Eq (HVoid f a)
deriving instance Ord (HVoid f a)

type family HCoproduct hs where
  HCoproduct '[] = HVoid
  HCoproduct (h ': hs) = h :+: HCoproduct hs

class Injective (h1 :: (k -> Type) -> k -> Type) h2 where
  hInject :: forall f. h1 f ~> h2 f
  hProject :: forall f a. h2 f a -> Maybe (h1 f a)

instance {-# OVERLAPPING #-} Injective h1 (h1 :+: h2) where
  hInject :: h1 f ~> (h1 :+: h2) f
  hInject = HInjL
  hProject :: (h1 :+: h2) f a -> Maybe (h1 f a)
  hProject (HInjL h) = Just h
  hProject (HInjR _) = Nothing

instance {-# OVERLAPPABLE #-} (Injective h1 h2) => Injective h1 (h3 :+: h2) where
  hInject :: h1 f ~> (h3 :+: h2) f
  hInject = HInjR . hInject
  hProject :: (h3 :+: h2) f a -> Maybe (h1 f a)
  hProject (HInjL _) = Nothing
  hProject (HInjR h) = hProject h

instance HFunctor HVoid where
  hmap _ v = case v of {}

instance (HFunctor h1, HFunctor h2) => HFunctor (h1 :+: h2) where
  hmap f (HInjL h) = HInjL (hmap f h)
  hmap f (HInjR h) = HInjR (hmap f h)

{-
  Helpers for safe-projecting from coproducts.
-}

class IsVoidIn (h :: (k -> Type) -> k -> Type) (a :: k) where
  hAbsurd :: forall f b. h f a -> b

instance IsVoidIn HVoid a where
  hAbsurd h = case h of {}

instance (IsVoidIn h1 a, IsVoidIn h2 a) => IsVoidIn (h1 :+: h2) a where
  hAbsurd :: forall f b. (h1 :+: h2) f a -> b
  hAbsurd (HInjL h1) = hAbsurd h1
  hAbsurd (HInjR h2) = hAbsurd h2

class SafeProjective h1 h2 a where
  hSafeProject :: h2 f a -> h1 f a

instance {-# OVERLAPPING #-} (h2 `IsVoidIn` a) => SafeProjective h1 (h2 :+: h1) a where
  hSafeProject :: (h2 :+: h1) f a -> h1 f a
  hSafeProject (HInjL h2) = hAbsurd h2
  hSafeProject (HInjR h1) = h1

instance {-# OVERLAPPABLE #-} (h3 `IsVoidIn` a, SafeProjective h1 h2 a) => SafeProjective h1 (h2 :+: h3) a where
  hSafeProject :: (h2 :+: h3) f a -> h1 f a
  hSafeProject (HInjL h2) = hSafeProject h2
  hSafeProject (HInjR h3) = hAbsurd h3
