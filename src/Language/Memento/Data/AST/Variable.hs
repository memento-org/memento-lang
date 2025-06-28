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

module Language.Memento.Data.AST.Variable (Variable (..), TypeVariable (..)) where

import           Data.Kind                                      (Type)
import           Data.Text                                      (Text)
import           Language.Memento.Data.AST.Tag                  (KTypeVariable,
                                                                 KVariable)
import           Language.Memento.Data.Functor.Coproduct.Higher (IsVoidIn (..), HInhabitOnly (..))
import           Language.Memento.Data.Functor.Higher           (HFunctor (hmap),
                                                                 HPhantom (..))
import           Language.Memento.Data.NaturalTransformation    (type (~>))
import           Language.Memento.Data.Type.NonEq               (type (/~))

data Variable (f :: Type -> Type) a where
  Var :: Text -> Variable f KVariable

data TypeVariable (f :: Type -> Type) a where
  TypeVar :: Text -> TypeVariable f KTypeVariable

deriving instance Show (Variable f a)
deriving instance Eq (Variable f a)
deriving instance Ord (Variable f a)

deriving instance Show (TypeVariable f a)
deriving instance Eq (TypeVariable f a)
deriving instance Ord (TypeVariable f a)

instance HFunctor Variable where
  hmap :: (f ~> g) -> Variable f ~> Variable g
  hmap _ = \case
    Var v -> Var v

instance HFunctor TypeVariable where
  hmap :: (f ~> g) -> TypeVariable f ~> TypeVariable g
  hmap _ = \case
    TypeVar v -> TypeVar v

instance (a /~ KVariable) => Variable `IsVoidIn` a where
  hAbsurd :: Variable f a -> b
  hAbsurd = \case {}

instance (a /~ KTypeVariable) => TypeVariable `IsVoidIn` a where
  hAbsurd :: TypeVariable f a -> b
  hAbsurd = \case {}

instance HPhantom Variable where
  hCoerce (Var v) = Var v

instance HPhantom TypeVariable where
  hCoerce (TypeVar v) = TypeVar v

instance HInhabitOnly Variable KVariable where
  hInhabitOnly = \case
    Var v -> Var v

instance HInhabitOnly TypeVariable KTypeVariable where
  hInhabitOnly = \case
    TypeVar v -> TypeVar v
