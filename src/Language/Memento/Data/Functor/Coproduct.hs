{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Memento.Data.Functor.Coproduct (
  (:+:) (..),
  VoidF,
  Injective (..),
  Coproduct,
  (?:),
  absurdVoidF,
) where

import           Data.Kind                                   (Type)
import           Language.Memento.Data.NaturalTransformation (type (~>))

data (f1 :+: f2) a
  = InjL (f1 a)
  | InjR (f2 a)

deriving instance (Show (f1 a), Show (f2 a)) => Show ((f1 :+: f2) a)
deriving instance (Eq (f1 a), Eq (f2 a)) => Eq ((f1 :+: f2) a)
deriving instance (Ord (f1 a), Ord (f2 a)) => Ord ((f1 :+: f2) a)

data VoidF a

deriving instance Show (VoidF a)
deriving instance Eq (VoidF a)
deriving instance Ord (VoidF a)

type family Coproduct hs where
  Coproduct '[] = VoidF
  Coproduct (h ': hs) = h :+: Coproduct hs

class Injective (f1 :: Type -> Type) f2 where
  inject :: f1 ~> f2
  project :: forall a. f2 a -> Maybe (f1 a)

instance {-# OVERLAPPING #-} Injective f1 (f1 :+: f2) where
  inject = InjL
  project = \case
    InjL h -> Just h
    InjR _ -> Nothing

instance {-# OVERLAPPABLE #-} (Injective h1 h2) => Injective h1 (h3 :+: h2) where
  inject = InjR . inject
  project (InjL _) = Nothing
  project (InjR h) = project h

instance Functor VoidF where
  fmap _ v = case v of {}

instance Foldable VoidF where
  foldMap _ v = case v of {}

instance Traversable VoidF where
  traverse _ v = case v of {}

instance (Functor f1, Functor f2) => Functor (f1 :+: f2) where
  fmap f (InjL h) = InjL (fmap f h)
  fmap f (InjR h) = InjR (fmap f h)

instance (Foldable f1, Foldable f2) => Foldable (f1 :+: f2) where
  foldMap f (InjL h) = foldMap f h
  foldMap f (InjR h) = foldMap f h

instance (Traversable f1, Traversable f2) => Traversable (f1 :+: f2) where
  traverse f (InjL h) = InjL <$> traverse f h
  traverse f (InjR h) = InjR <$> traverse f h

-- | Combinator for building coproduct handlers
(?:) :: (f1 a -> r) -> (f2 a -> r) -> ((f1 :+: f2) a -> r)
(?:) handler restHandler = \case
  InjL fa -> handler fa
  InjR rest -> restHandler rest

infixr 5 ?:

-- | Handler for empty coproduct (impossible case)
absurdVoidF :: VoidF a -> r
absurdVoidF voidF = case voidF of {}
