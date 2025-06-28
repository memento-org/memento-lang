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

module Language.Memento.Data.AST.Definition (ConstructorDef (..), Definition (..), SyntaxVariance (..)) where

import           Data.Bifunctor                                 (Bifunctor (bimap))
import           Data.Functor                                   ((<&>))
import           Data.Kind                                      (Type)
import           GHC.Base                                       (List)
import           Language.Memento.Data.AST.Tag                  (KBlock,
                                                                 KDefinition,
                                                                 KExpr, KType,
                                                                 KTypeVariable,
                                                                 KVariable)
import           Language.Memento.Data.Functor.Coproduct.Higher (HInhabitOnly (..),
                                                                 IsVoidIn (..))
import           Language.Memento.Data.Functor.Higher           (HFunctor (hmap))
import           Language.Memento.Data.NaturalTransformation    (type (~>))
import           Language.Memento.Data.Type.NonEq               (type (/~))

-- Individual constructor definition for multi-constructor data types
data ConstructorDef f
  = ConstructorDef
       (f KVariable) -- Constructor name (e.g., None)
      (List (f KTypeVariable)) -- Constructor generics (e.g., [T])
      (List (f KVariable, f KType)) -- Function arguments (e.g., [T])
      (f KType) -- Return type (e.g., Maybe<T>)

deriving instance (Show (f KVariable), Show (f KTypeVariable), Show (f KType)) => Show (ConstructorDef f)
deriving instance (Eq (f KVariable), Eq (f KTypeVariable), Eq (f KType)) => Eq (ConstructorDef f)
deriving instance (Ord (f KVariable), Ord (f KTypeVariable), Ord (f KType)) => Ord (ConstructorDef f)

data Definition (f :: Type -> Type) a where
  ValDef ::
    f KVariable -> -- Variable name
    List (f KTypeVariable) -> -- Type parameters (e.g., [T, U])
    f KType -> -- Type annotation
    f KExpr -> -- Expression
    Definition f KDefinition
  FnDef ::
    f KVariable -> -- Function name
    List (f KTypeVariable) -> -- Type parameters (e.g., [T, U])
    List (f KVariable, f KType) -> -- Function arguments (e.g., [x : T, xs : List<T>])
    f KType -> -- Return type
    f KBlock -> -- Function body
    Definition f KDefinition
  -- Multi-constructor data definition syntax:
  -- data Maybe [Some<T> : (x : T) => Maybe<T>, None<T> : () => Maybe<T>];
  DataDef ::
    f KVariable -> -- Data type name (e.g., Maybe)
    List SyntaxVariance ->
    List (ConstructorDef f) -> -- List of constructors
    Definition f KDefinition
  TypeDef ::
    f KVariable -> -- Type alias name
    List (f KTypeVariable) -> -- Type parameters (e.g., [T, U])
    f KType -> -- Type definition
    Definition f KDefinition

deriving instance (Show (f KVariable), Show (f KTypeVariable), Show (f KType), Show (f KExpr), Show (f KBlock)) => Show (Definition f a)
deriving instance (Eq (f KVariable), Eq (f KTypeVariable), Eq (f KType), Eq (f KExpr), Eq (f KBlock)) => Eq (Definition f a)
deriving instance (Ord (f KVariable), Ord (f KTypeVariable), Ord (f KType), Ord (f KExpr), Ord (f KBlock)) => Ord (Definition f a)

data SyntaxVariance
  = SVAuto    -- ^ Auto variance, e.g., `data Option<auto> { ... }`
  | SVIn      -- ^ In variance, e.g., `data Consumer<in> { ... }`
  | SVOut     -- ^ Out variance, e.g., `data Producer<out> { ... }`
  | SVInOut   -- ^ InOut variance, e.g., `data BiConsumer<inout> { ... }`
  | SVPhantom -- ^ Phantom variance, e.g., `data Phantom<phantom> { ... }`
  deriving (Show, Eq, Ord)

instance HFunctor Definition where
  hmap :: (f ~> g) -> Definition f ~> Definition g
  hmap f = \case
    ValDef v params t e -> ValDef (f v) (map f params) (f t) (f e)
    FnDef v params args t e ->
      FnDef (f v) (map f params) (args <&> bimap f f) (f t) (f e)
    DataDef dataName vs  constructors ->
      DataDef
        (f dataName)
        vs
        ( map
            ( \case
                ConstructorDef name typeParams args t ->
                  ConstructorDef (f name) (map f typeParams) (args <&> bimap f f) (f t)
            )
            constructors
        )
    TypeDef v params t -> TypeDef (f v) (map f params) (f t)

instance (a /~ KDefinition) => Definition `IsVoidIn` a where
  hAbsurd :: Definition f a -> b
  hAbsurd = \case {}

instance HInhabitOnly Definition KDefinition where
  hInhabitOnly :: forall f b. Definition f b -> Definition f KDefinition
  hInhabitOnly = \case
    ValDef v params t e -> ValDef v params t e
    FnDef v params args t e -> FnDef v params args t e
    DataDef dataName vs constructors -> DataDef dataName vs constructors
    TypeDef v params t -> TypeDef v params t
