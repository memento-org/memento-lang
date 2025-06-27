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

module Language.Memento.Data.AST.Literal (Literal (..)) where

import Data.Kind (Type)
import Data.Text (Text)
import Language.Memento.Data.AST.Tag (KLiteral)
import Language.Memento.Data.Functor.Coproduct.Higher (IsVoidIn (..))
import Language.Memento.Data.Functor.Higher (HFunctor (hmap))
import Language.Memento.Data.NaturalTransformation (type (~>))
import Language.Memento.Data.Type.NonEq (type (/~))

data Literal (f :: Type -> Type) a where
  NumberLiteral :: Double -> Literal f KLiteral
  BoolLiteral :: Bool -> Literal f KLiteral
  StringLiteral :: Text -> Literal f KLiteral
  IntLiteral :: Int -> Literal f KLiteral

deriving instance Show (Literal f a)
deriving instance Eq (Literal f a)
deriving instance Ord (Literal f a)

instance HFunctor Literal where
  hmap :: (f ~> g) -> Literal f ~> Literal g
  hmap _ = \case
    NumberLiteral n -> NumberLiteral n
    BoolLiteral b -> BoolLiteral b
    StringLiteral s -> StringLiteral s
    IntLiteral i -> IntLiteral i

instance (a /~ KLiteral) => Literal `IsVoidIn` a where
  hAbsurd :: Literal f a -> b
  hAbsurd = \case {}
