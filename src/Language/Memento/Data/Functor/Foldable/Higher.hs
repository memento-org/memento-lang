{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
module Language.Memento.Data.Functor.Foldable.Higher (HFoldable (..)) where
import           GHC.Base (Type)

class HFoldable (h :: (k -> Type) -> k -> Type) where
  -- | Fold over a higher-kinded functor
  hfoldr :: forall f a r. (forall x. f x -> r -> r) -> h f a -> r -> r
