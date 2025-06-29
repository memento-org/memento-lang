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

module Language.Memento.Data.AST.Expr (Expr (..), Let (..), BinOp(..), Block (..)) where

import           Data.Bifunctor                                 (Bifunctor (bimap))
import           Data.Kind                                      (Type)
import           GHC.Base                                       (List)
import           Language.Memento.Data.AST.Tag                  (KBlock, KExpr,
                                                                 KLet, KLiteral,
                                                                 KPattern,
                                                                 KType,
                                                                 KVariable)
import           Language.Memento.Data.Functor.Coproduct.Higher (IsVoidIn (..), HInhabitOnly (..))
import           Language.Memento.Data.Functor.Higher           (HFunctor (hmap))
import           Language.Memento.Data.NaturalTransformation    (type (~>))
import           Language.Memento.Data.Type.NonEq               (type (/~))

data BinOp where
  Add :: BinOp
  Sub :: BinOp
  Mul :: BinOp
  Div :: BinOp
  Eq :: BinOp
  Lt :: BinOp
  Gt :: BinOp
  deriving (Show, Eq, Ord)

data Expr (f :: Type -> Type) a where
  EVar :: f KVariable -> Expr f KExpr
  ELiteral :: f KLiteral -> Expr f KExpr
  ELambda :: List (f KPattern, Maybe (f KType)) -> f KExpr -> Expr f KExpr
  EApply :: f KExpr -> List (f KExpr) -> Expr f KExpr
  EMatch :: List (f KExpr) -> List (List (f KPattern, Maybe (f KType)), f KExpr) -> Expr f KExpr -- Match はλ式の集合
  EIf :: f KExpr -> f KExpr -> f KExpr -> Expr f KExpr
  EBinOp :: BinOp  -> f KExpr -> f KExpr -> Expr f KExpr
  EBlock :: f KBlock -> Expr f KExpr

deriving instance
  ( Show (f KLet)
  , Show (f KVariable)
  , Show (f KLiteral)
  , Show (f KPattern)
  , Show (f KType)
  , Show (f KExpr)
  , Show (f KBlock)
  ) =>
  Show (Expr f a)
deriving instance
  ( Eq (f KLet)
  , Eq (f KVariable)
  , Eq (f KLiteral)
  , Eq (f KPattern)
  , Eq (f KType)
  , Eq (f KExpr)
  , Eq (f KBlock)
  ) =>
  Eq (Expr f a)
deriving instance
  ( Ord (f KLet)
  , Ord (f KVariable)
  , Ord (f KLiteral)
  , Ord (f KPattern)
  , Ord (f KType)
  , Ord (f KExpr)
  , Ord (f KBlock)
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
    ELambda ps e -> ELambda (map (bimap f (fmap f)) ps) (f e)
    EApply e es -> EApply (f e) (map f es)
    EMatch es cs -> EMatch (map f es) (map (bimap (map (bimap f (fmap f))) f) cs)
    EIf e1 e2 e3 -> EIf (f e1) (f e2) (f e3)
    EBinOp op e1 e2 -> EBinOp op (f e1) (f e2)
    EBlock b -> EBlock (f b)

instance HFunctor Let where
  hmap :: (f ~> g) -> Let f ~> Let g
  hmap f = \case
    Let p mt e -> Let (f p) (fmap f mt) (f e)

instance (a /~ KExpr) => Expr `IsVoidIn` a where
  hAbsurd :: Expr f a -> b
  hAbsurd = \case {}

instance (a /~ KLet) => Let `IsVoidIn` a where
  hAbsurd :: Let f a -> b
  hAbsurd = \case {}

data Block (f :: Type -> Type) a where
  Block :: List (f KLet) -> f KExpr -> Block f KBlock

deriving instance
  ( Show (f KLet)
  , Show (f KExpr)
  ) =>
  Show (Block f a)
deriving instance
  ( Eq (f KLet)
  , Eq (f KExpr)
  ) =>
  Eq (Block f a)
deriving instance
  ( Ord (f KLet)
  , Ord (f KExpr)
  ) =>
  Ord (Block f a)

instance HFunctor Block where
  hmap :: (f ~> g) -> Block f ~> Block g
  hmap f = \case
    Block lets expr -> Block (map f lets) (f expr)

instance (a /~ KBlock) => Block `IsVoidIn` a where
  hAbsurd :: Block f a -> b
  hAbsurd = \case {}

instance HInhabitOnly Expr KExpr where
  hInhabitOnly = \case
    EVar v -> EVar v
    ELiteral l -> ELiteral l
    ELambda ps e -> ELambda ps e
    EApply e es -> EApply e es
    EMatch es cs -> EMatch es cs
    EIf e1 e2 e3 -> EIf e1 e2 e3
    EBinOp op e1 e2 -> EBinOp op e1 e2
    EBlock b -> EBlock b

instance HInhabitOnly Let KLet where
  hInhabitOnly = \case
    Let p mt e -> Let p mt e

instance HInhabitOnly Block KBlock where
  hInhabitOnly = \case
    Block lets expr -> Block lets expr
