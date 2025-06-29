{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Data.Functor.FixedPoint (Fix (..), extractFix, foldFix, injectFix, foldFixM, projectFix) where

import           Language.Memento.Data.Functor.Coproduct (Injective (..))
import           Language.Memento.Data.Functor.Product   (Extractive (extract))

newtype Fix f = Fix {unFix :: f (Fix f)}

deriving instance (Show (f (Fix f))) => Show (Fix f)
deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Ord (f (Fix f))) => Ord (Fix f)

projectFix :: (Injective f f') => Fix f' -> Maybe (f (Fix f'))
projectFix (Fix f) = project f

extractFix :: (Extractive f f') => Fix f' -> f (Fix f')
extractFix (Fix f) = extract f

injectFix :: (Injective f f') => f (Fix f') -> Fix f'
injectFix f = Fix {unFix = inject f}

foldFix :: (Functor f) => (f a -> a) -> Fix f -> a
foldFix f (Fix x) = f (fmap (foldFix f) x)

foldFixM :: (Monad m, Traversable f) => (f a -> m a) -> Fix f -> m a
foldFixM f (Fix x) = traverse (foldFixM f) x >>= f
