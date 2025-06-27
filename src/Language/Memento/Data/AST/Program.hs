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

module Language.Memento.Data.AST.Program (Program (..)) where

import GHC.Base
import Language.Memento.Data.AST.Tag (KDefinition, KProgram)
import Language.Memento.Data.Functor.Coproduct.Higher (IsVoidIn (..))
import Language.Memento.Data.Functor.Higher (HFunctor (hmap))
import Language.Memento.Data.NaturalTransformation (type (~>))
import Language.Memento.Data.Type.NonEq (type (/~))

data Program (f :: Type -> Type) a where
  Program :: List (f KDefinition) -> Program f KProgram

deriving instance (Show (f KDefinition)) => Show (Program f a)
deriving instance (Eq (f KDefinition)) => Eq (Program f a)
deriving instance (Ord (f KDefinition)) => Ord (Program f a)

instance HFunctor Program where
  hmap :: (f ~> g) -> Program f ~> Program g
  hmap f = \case
    Program ds -> Program (map f ds)

instance (a /~ KProgram) => Program `IsVoidIn` a where
  hAbsurd :: Program f a -> b
  hAbsurd = \case {}
