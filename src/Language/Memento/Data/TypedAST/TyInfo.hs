{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Memento.Data.TypedAST.TyInfo (
  LiteralTyInfo(..),
  VariableTyInfo(..),
  TypeVariableTyInfo(..),
  TypeTyInfo(..),
  DefinitionTyInfo(..),
  LetTyInfo(..),
  ExprTyInfo(..),
  BlockTyInfo(..),
  PatternTyInfo(..),
  ProgramTyInfo(..),
  -- Transformation functions
  transformLiteralTyInfo,
  transformVariableTyInfo,
  transformTypeVariableTyInfo,
  transformTypeTyInfo,
  transformDefinitionTyInfo,
  transformLetTyInfo,
  transformExprTyInfo,
  transformBlockTyInfo,
  transformPatternTyInfo,
  transformProgramTyInfo
) where

import           GHC.Base                                       (Type)
import           Language.Memento.Data.AST.Tag                  (KBlock,
                                                                 KDefinition,
                                                                 KExpr, KLet,
                                                                 KLiteral,
                                                                 KPattern,
                                                                 KProgram,
                                                                 KType,
                                                                 KTypeVariable,
                                                                 KVariable)
import           Language.Memento.Data.Functor.Coproduct.Higher (IsVoidIn (hAbsurd))
import           Language.Memento.Data.Functor.Higher           (HFunctor (hmap))
import           Language.Memento.Data.Ty                       ()
import           Language.Memento.Data.Type.NonEq               (type (/~))

data LiteralTyInfo t (f :: Type -> Type) a where
  -- | Literal's type
  LiteralTyInfo :: t -> LiteralTyInfo t f KLiteral

deriving instance (Show t) => Show (LiteralTyInfo t f a)
deriving instance (Eq t) => Eq (LiteralTyInfo t f a)
deriving instance (Ord t) => Ord (LiteralTyInfo t f a)

instance HFunctor (LiteralTyInfo t) where
  hmap _ (LiteralTyInfo t) = LiteralTyInfo t

instance (a /~ KLiteral) => LiteralTyInfo t `IsVoidIn` a where
  hAbsurd :: LiteralTyInfo t f a -> b
  hAbsurd = \case {}

data VariableTyInfo t (f :: Type -> Type) a where
  -- | Variables's type
  VariableTyInfo :: t -> VariableTyInfo t f KVariable

deriving instance (Show t) => Show (VariableTyInfo t f a)
deriving instance (Eq t) => Eq (VariableTyInfo t f a)
deriving instance (Ord t) => Ord (VariableTyInfo t f a)

instance HFunctor (VariableTyInfo t) where
  hmap _ (VariableTyInfo t) = VariableTyInfo t

instance (a /~ KVariable) => VariableTyInfo t `IsVoidIn` a where
  hAbsurd :: VariableTyInfo t f a -> b
  hAbsurd = \case {}

data TypeVariableTyInfo t (f :: Type -> Type) a where
  -- | TypeVariable itself don't contain any type information
  TypeVariableTyInfo :: TypeVariableTyInfo t f KTypeVariable

deriving instance Show (TypeVariableTyInfo t f a)
deriving instance Eq (TypeVariableTyInfo t f a)
deriving instance Ord (TypeVariableTyInfo t f a)

instance HFunctor (TypeVariableTyInfo t) where
  hmap _ TypeVariableTyInfo = TypeVariableTyInfo

instance (a /~ KTypeVariable) => TypeVariableTyInfo t `IsVoidIn` a where
  hAbsurd :: TypeVariableTyInfo t f a -> b
  hAbsurd = \case {}

data TypeTyInfo t (f :: Type -> Type) a where
  -- | Type itself don't contain any type information
  TypeTyInfo :: TypeTyInfo t f KType

deriving instance Show (TypeTyInfo t f a)
deriving instance Eq (TypeTyInfo t f a)
deriving instance Ord (TypeTyInfo t f a)

instance HFunctor (TypeTyInfo t) where
  hmap _ TypeTyInfo = TypeTyInfo

instance (a /~ KType) => TypeTyInfo t `IsVoidIn` a where
  hAbsurd :: TypeTyInfo t f a -> b
  hAbsurd = \case {}

data DefinitionTyInfo t (f :: Type -> Type) a where
  -- | value's type schema
  -- | value's name & its type schema
  DefinitionTyInfo :: DefinitionTyInfo t f KDefinition

deriving instance Show (DefinitionTyInfo t f a)
deriving instance Eq (DefinitionTyInfo t f a)
deriving instance Ord (DefinitionTyInfo t f a)

instance HFunctor (DefinitionTyInfo t) where
  hmap _ DefinitionTyInfo = DefinitionTyInfo

instance (a /~ KDefinition) => DefinitionTyInfo t `IsVoidIn` a where
  hAbsurd :: DefinitionTyInfo t f a -> b
  hAbsurd = \case {}

data LetTyInfo t (f :: Type -> Type) a where
  -- | let binding's type
  -- | e.g.  for `let x : t = e;`, store "t"
  LetTyInfo :: t -> LetTyInfo t f KLet

deriving instance (Show t) => Show (LetTyInfo t f a)
deriving instance (Eq t) => Eq (LetTyInfo t f a)
deriving instance (Ord t) => Ord (LetTyInfo t f a)

instance HFunctor (LetTyInfo t) where
  hmap _ (LetTyInfo t) = LetTyInfo t

instance (a /~ KLet) => LetTyInfo t `IsVoidIn` a where
  hAbsurd :: LetTyInfo t f a -> b
  hAbsurd = \case {}

data ExprTyInfo t (f :: Type -> Type) a where
  -- | expression's type
  ExprTyInfo :: t -> ExprTyInfo t f KExpr

deriving instance (Show t) => Show (ExprTyInfo t f a)
deriving instance (Eq t) => Eq (ExprTyInfo t f a)
deriving instance (Ord t) => Ord (ExprTyInfo t f a)

instance HFunctor (ExprTyInfo t) where
  hmap _ (ExprTyInfo t) = ExprTyInfo t

instance (a /~ KExpr) => ExprTyInfo t `IsVoidIn` a where
  hAbsurd :: ExprTyInfo t f a -> b
  hAbsurd = \case {}

data BlockTyInfo t (f :: Type -> Type) a where
  -- | block's type
  BlockTyInfo :: t -> BlockTyInfo t f KBlock

deriving instance (Show t) => Show (BlockTyInfo t f a)
deriving instance (Eq t) => Eq (BlockTyInfo t f a)
deriving instance (Ord t) => Ord (BlockTyInfo t f a)

instance HFunctor (BlockTyInfo t) where
  hmap _ (BlockTyInfo t) = BlockTyInfo t

instance (a /~ KBlock) => BlockTyInfo t `IsVoidIn` a where
  hAbsurd :: BlockTyInfo t f a -> b
  hAbsurd = \case {}

data PatternTyInfo t (f :: Type -> Type) a where
  -- | Pattern's type and its expected type
  -- | Note : e.g. `fn (Some(x) : unknown) -> e;`, `unknown` is a pattern type info,
  -- | which generates assumption Option<T> <: unknown (from Some's type)
  -- |
  -- | `data Consume [fn mk<U, T>(value : U, (callback : fn (value : U) -> T)) -> Consume<T>]
  -- | `switch (x) { case (mk(value, callback)) -> callback(value), }
  -- |
  -- | this is annotated by
  -- |
  -- | i. pattern (mk(value, callback)) <== $t0
  -- | ii. pattern `mk` <== fn (value: U', callback: fn (value: U') -> T') -> Consume<T'> -- renewed generics
  -- | iii. pattern value <== $t1 -- renewed type var (in assumption)
  -- | iv. pattern callback <== $t2
  -- | v. switched scrutinee x <== $t3 -- renewed type variables
  -- | vi. variable callback <== $t2 -- matched by pattern
  -- | vii. variable value <== $t1 -- matched by pattern
  -- | viii. expression callback(value)  <== $t4
  -- | ix. expression switch ...   <== $t5
  -- |
  -- | This will be generate assumptions & constraints like
  -- | ASSUMPTION:  (fn (U', fn (U') -> T') -> Consume<T'>) <: (fn ($t1, $t2) -> $t0)  (by i & ii & iii & iv)
  -- | CONSTRAINT:  $t3 <: $t0  (by i & v)
  -- | CONSTRAINT:  (fn ($t1) -> $t4) <: $t2  (by vi & vii & viii)
  -- | CONSTRAINT:  $t4 <: $t5  (by viii & ix)
  PatternTyInfo :: t ->  PatternTyInfo t f KPattern

deriving instance (Show t) => Show (PatternTyInfo t f a)
deriving instance (Eq t) => Eq (PatternTyInfo t f a)
deriving instance (Ord t) => Ord (PatternTyInfo t f a)

instance HFunctor (PatternTyInfo t) where
  hmap _ (PatternTyInfo t) = PatternTyInfo t

instance (a /~ KPattern) => PatternTyInfo t `IsVoidIn` a where
  hAbsurd :: PatternTyInfo t f a -> b
  hAbsurd = \case {}

data ProgramTyInfo t (f :: Type -> Type) a where
  -- | Program's type
  ProgramTyInfo :: ProgramTyInfo t f KProgram

deriving instance Show (ProgramTyInfo t f a)
deriving instance Eq (ProgramTyInfo t f a)
deriving instance Ord (ProgramTyInfo t f a)

instance HFunctor (ProgramTyInfo t) where
  hmap _ ProgramTyInfo = ProgramTyInfo

instance (a /~ KProgram) => ProgramTyInfo t `IsVoidIn` a where
  hAbsurd :: ProgramTyInfo t f a -> b
  hAbsurd = \case {}

-- Transformation functions

transformLiteralTyInfo :: (t -> t') -> LiteralTyInfo t f a -> LiteralTyInfo t' f' a
transformLiteralTyInfo ft (LiteralTyInfo t) = LiteralTyInfo (ft t)

transformVariableTyInfo :: (t -> t') -> VariableTyInfo t f a -> VariableTyInfo t' f' a
transformVariableTyInfo ft (VariableTyInfo t) = VariableTyInfo (ft t)

transformTypeVariableTyInfo :: (t -> t') -> TypeVariableTyInfo t f a -> TypeVariableTyInfo t' f' a
transformTypeVariableTyInfo _ TypeVariableTyInfo = TypeVariableTyInfo

transformTypeTyInfo :: (t -> t') -> TypeTyInfo t f a -> TypeTyInfo t' f' a
transformTypeTyInfo _ TypeTyInfo = TypeTyInfo

transformDefinitionTyInfo :: (t -> t') -> DefinitionTyInfo t f a -> DefinitionTyInfo t' f' a
transformDefinitionTyInfo _ DefinitionTyInfo = DefinitionTyInfo

transformLetTyInfo :: (t -> t') -> LetTyInfo t f a -> LetTyInfo t' f' a
transformLetTyInfo ft (LetTyInfo t) = LetTyInfo (ft t)

transformExprTyInfo :: (t -> t') -> ExprTyInfo t f a -> ExprTyInfo t' f' a
transformExprTyInfo ft (ExprTyInfo t) = ExprTyInfo (ft t)

transformBlockTyInfo :: (t -> t') -> BlockTyInfo t f a -> BlockTyInfo t' f' a
transformBlockTyInfo ft (BlockTyInfo t) = BlockTyInfo (ft t)

transformPatternTyInfo :: (t -> t') -> PatternTyInfo t f a -> PatternTyInfo t' f' a
transformPatternTyInfo ft (PatternTyInfo t) = PatternTyInfo (ft t)

transformProgramTyInfo :: (t -> t') -> ProgramTyInfo t f a -> ProgramTyInfo t' f' a
transformProgramTyInfo _ ProgramTyInfo = ProgramTyInfo
