{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Data.Functor.FixedPoint (Fix (..), extractFix, foldFix) where

import           Language.Memento.Data.Functor.Product (Extractive (extract))

newtype Fix f = Fix {unFix :: f (Fix f)}

deriving instance (Show (f (Fix f))) => Show (Fix f)
deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Ord (f (Fix f))) => Ord (Fix f)

extractFix :: (Extractive f f') => Fix f' -> f (Fix f')
extractFix (Fix f) = extract f

foldFix :: (Functor f) => (f a -> a) -> Fix f -> a
foldFix f (Fix x) = f (fmap (foldFix f) x)
