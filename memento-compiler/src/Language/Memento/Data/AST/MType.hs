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

module Language.Memento.Data.AST.MType (MType (..)) where

import           Data.Bifunctor                                 (Bifunctor (bimap))
import           Data.Kind                                      (Type)
import           GHC.Base                                       (List)
import           Language.Memento.Data.AST.Tag                  (KLiteral,
                                                                 KType,
                                                                 KTypeVariable,
                                                                 KVariable)
import           Language.Memento.Data.Functor.Coproduct.Higher (IsVoidIn (..), HInhabitOnly (..))
import           Language.Memento.Data.Functor.Higher           (HFunctor (hmap))
import           Language.Memento.Data.NaturalTransformation    (type (~>))
import           Language.Memento.Data.Type.NonEq               (type (/~))

-- | Type is a type-level function that maps a kind to a type.
data MType (f :: Type -> Type) a where
  TVar :: f KTypeVariable -> MType f KType
  TNumber :: MType f KType
  TInt :: MType f KType
  TBool :: MType f KType
  TString :: MType f KType
  TFunction ::
    List (f KVariable, f KType) -> f KType -> MType f KType
  TUnknown :: MType f KType
  TNever :: MType f KType
  TLiteral :: f KLiteral -> MType f KType
  TUnion :: List (f KType) -> MType f KType
  TIntersection :: List (f KType) -> MType f KType
  -- Polymorphism support
  TApplication :: f KTypeVariable -> List (f KType) -> MType f KType -- Type<Arg1, Arg2, ...>

deriving instance (Show (f KLiteral), Show (f KType), Show (f KVariable), Show (f KTypeVariable)) => Show (MType f a)
deriving instance (Eq (f KLiteral), Eq (f KType), Eq (f KVariable), Eq (f KTypeVariable)) => Eq (MType f a)
deriving instance (Ord (f KLiteral), Ord (f KType), Ord (f KVariable), Ord (f KTypeVariable)) => Ord (MType f a)

instance HFunctor MType where
  hmap :: (f ~> g) -> MType f ~> MType g
  hmap f = \case
    TVar v -> TVar (f v)
    TNumber -> TNumber
    TInt -> TInt
    TBool -> TBool
    TString -> TString
    TFunction ts e -> TFunction (map (bimap f f) ts) (f e)
    TUnknown -> TUnknown
    TNever -> TNever
    TLiteral l -> TLiteral (f l)
    TUnion ts -> TUnion (map f ts)
    TIntersection ts -> TIntersection (map f ts)
    TApplication base args -> TApplication (f base) (map f args)

instance (a /~ KType) => MType `IsVoidIn` a where
  hAbsurd :: MType f a -> b
  hAbsurd = \case {}

instance HInhabitOnly MType KType where
  hInhabitOnly = \case
    TVar v -> TVar v
    TNumber -> TNumber
    TInt -> TInt
    TBool -> TBool
    TString -> TString
    TFunction ts e -> TFunction ts e
    TUnknown -> TUnknown
    TNever -> TNever
    TLiteral l -> TLiteral l
    TUnion ts -> TUnion ts
    TIntersection ts -> TIntersection ts
    TApplication base args -> TApplication base args
