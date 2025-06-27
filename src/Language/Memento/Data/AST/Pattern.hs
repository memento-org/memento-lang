{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Memento.Data.AST.Pattern (Pattern (..)) where

import           Data.Kind                                      (Type)
import           GHC.Base                                       (List)
import           Language.Memento.Data.AST.Tag                  (KLiteral,
                                                                 KPattern,
                                                                 KVariable)
import           Language.Memento.Data.Functor.Coproduct.Higher (IsVoidIn (..))
import           Language.Memento.Data.Functor.Higher           (HFunctor (hmap))
import           Language.Memento.Data.NaturalTransformation    (type (~>))
import           Language.Memento.Data.Type.NonEq               (type (/~))

-- | Pattern represents a pattern in pattern matching
data Pattern (f :: Type -> Type) (a :: Type) where
  -- | Variable pattern matches any value and binds it to a name
  PVar :: f KVariable -> Pattern f KPattern
  -- | Wildcard pattern matches any value without binding
  PWildcard :: Pattern f KPattern
  -- | Literal patterns match exact values
  PLiteral :: f KLiteral -> Pattern f KPattern
  -- | Constructor pattern matches a specific constructor and its arguments
  PCons :: f KVariable -> List (f KPattern) -> Pattern f KPattern

deriving instance (Show (f KVariable), Show (f KLiteral), Show (f KPattern)) => Show (Pattern f a)
deriving instance (Eq (f KVariable), Eq (f KLiteral), Eq (f KPattern)) => Eq (Pattern f a)
deriving instance (Ord (f KVariable), Ord (f KLiteral), Ord (f KPattern)) => Ord (Pattern f a)

instance HFunctor Pattern where
  hmap :: (f ~> g) -> Pattern f ~> Pattern g
  hmap f = \case
    PVar v -> PVar (f v)
    PWildcard -> PWildcard
    PLiteral l -> PLiteral (f l)
    PCons v ps -> PCons (f v) (map f ps)

instance (a /~ KPattern) => Pattern `IsVoidIn` a where
  hAbsurd :: Pattern f a -> b
  hAbsurd = \case {}
