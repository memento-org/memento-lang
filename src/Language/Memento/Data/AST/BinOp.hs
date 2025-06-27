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

module Language.Memento.Data.AST.BinOp (BinOp (..)) where

import Data.Kind (Type)
import Language.Memento.Data.AST.Tag (KBinOp)
import Language.Memento.Data.Functor.Coproduct.Higher (IsVoidIn (..))
import Language.Memento.Data.Functor.Higher (HFunctor (hmap))
import Language.Memento.Data.NaturalTransformation (type (~>))
import Language.Memento.Data.Type.NonEq (type (/~))

data BinOp (f :: Type -> Type) a where
  Add :: BinOp f KBinOp
  Sub :: BinOp f KBinOp
  Mul :: BinOp f KBinOp
  Div :: BinOp f KBinOp
  Eq :: BinOp f KBinOp
  Lt :: BinOp f KBinOp
  Gt :: BinOp f KBinOp

deriving instance Show (BinOp f a)
deriving instance Eq (BinOp f a)
deriving instance Ord (BinOp f a)
instance HFunctor BinOp where
  hmap :: (f ~> g) -> BinOp f ~> BinOp g
  hmap _ = \case
    Add -> Add
    Sub -> Sub
    Mul -> Mul
    Div -> Div
    Eq -> Eq
    Lt -> Lt
    Gt -> Gt

instance (a /~ KBinOp) => BinOp `IsVoidIn` a where
  hAbsurd :: BinOp f a -> b
  hAbsurd = \case {}
