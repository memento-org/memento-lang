{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Data.AST.Expr (Expr (..), Let (..)) where

import Data.Kind (Type)
import GHC.Base (List)
import Language.Memento.Data.AST.Tag (KBinOp, KBlock, KExpr, KLet, KLiteral, KPattern, KType, KVariable)
import Language.Memento.Data.Functor.Coproduct.Higher (IsVoidIn (..))
import Language.Memento.Data.Functor.Higher (HFunctor (hmap))
import Language.Memento.Data.NaturalTransformation (type (~>))
import Language.Memento.Data.Type.NonEq (type (/~))

data Expr (f :: Type -> Type) a where
  EVar :: f KVariable -> Expr f KExpr
  ELiteral :: f KLiteral -> Expr f KExpr
  ELambda :: List (f KPattern, Maybe (f KType)) -> f KExpr -> Expr f KExpr
  EApply :: f KExpr -> List (f KExpr) -> Expr f KExpr
  EMatch :: List (f KExpr) -> List ([(f KPattern, Maybe (f KType))], f KExpr) -> Expr f KExpr -- Match はλ式の集合
  EIf :: f KExpr -> f KExpr -> f KExpr -> Expr f KExpr
  EBinOp :: f KBinOp -> f KExpr -> f KExpr -> Expr f KExpr
  EBlock :: List (f KLet) -> f KExpr -> Expr f KBlock

deriving instance
  ( Show (f KBinOp)
  , Show (f KLet)
  , Show (f KVariable)
  , Show (f KLiteral)
  , Show (f KPattern)
  , Show (f KType)
  , Show (f KExpr)
  ) =>
  Show (Expr f a)
deriving instance
  ( Eq (f KBinOp)
  , Eq (f KLet)
  , Eq (f KVariable)
  , Eq (f KLiteral)
  , Eq (f KPattern)
  , Eq (f KType)
  , Eq (f KExpr)
  ) =>
  Eq (Expr f a)
deriving instance
  ( Ord (f KBinOp)
  , Ord (f KLet)
  , Ord (f KVariable)
  , Ord (f KLiteral)
  , Ord (f KPattern)
  , Ord (f KType)
  , Ord (f KExpr)
  ) =>
  Ord (Expr f a)

data Let (f :: Type -> Type) a where
  Let :: f KPattern -> Maybe (f KType) -> f KExpr -> Let f KLet

deriving instance
  ( Show (f KPattern)
  , Show (f KType)
  , Show (f KExpr)
  ) =>
  Show (Let f a)
deriving instance
  ( Eq (f KPattern)
  , Eq (f KType)
  , Eq (f KExpr)
  ) =>
  Eq (Let f a)
deriving instance
  ( Ord (f KPattern)
  , Ord (f KType)
  , Ord (f KExpr)
  ) =>
  Ord (Let f a)

instance HFunctor Expr where
  hmap :: (f ~> g) -> Expr f ~> Expr g
  hmap f = \case
    EVar v -> EVar (f v)
    ELiteral l -> ELiteral (f l)
    ELambda ps e -> ELambda (map (\(p, mt) -> (f p, fmap f mt)) ps) (f e)
    EApply e es -> EApply (f e) (map f es)
    EMatch es cs -> EMatch (map f es) (map (\(ps, e) -> (map (\(p, mt) -> (f p, fmap f mt)) ps, f e)) cs)
    EIf e1 e2 e3 -> EIf (f e1) (f e2) (f e3)
    EBinOp op e1 e2 -> EBinOp (f op) (f e1) (f e2)
    EBlock ls e -> EBlock (map f ls) (f e)

instance HFunctor Let where
  hmap :: (f ~> g) -> Let f ~> Let g
  hmap f = \case
    Let p mt e -> Let (f p) (fmap f mt) (f e)

instance (a /~ KExpr, a /~ KBlock) => Expr `IsVoidIn` a where
  hAbsurd :: Expr f a -> b
  hAbsurd = \case {}

instance (a /~ KLet) => Let `IsVoidIn` a where
  hAbsurd :: Let f a -> b
  hAbsurd = \case {}
