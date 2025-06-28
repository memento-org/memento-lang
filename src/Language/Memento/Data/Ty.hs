{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Language.Memento.Data.Ty (TyF (..), TyVarF (..), Literal (..), Ty, UnsolvedTy, TyVariable, TyGeneric, TyConstructor, formatUnsolvedTy, formatTy, TypeScheme (..), formatTypeScheme, substituteGenerics, assignTypeToScheme, unsolvedTyToTy, substituteTyVar, typeVars, tyToUnsolvedTy) where

import           Data.Foldable                            (fold)
import qualified Data.Map                                 as Map
import           Data.Set                                 (Set)
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           GHC.Base                                 (List)
import           Language.Memento.Data.Functor.Coproduct  (Coproduct, Injective,
                                                           absurdVoidF, (?:))
import           Language.Memento.Data.Functor.FixedPoint (Fix (..), foldFix,
                                                           foldFixM, injectFix)

-- Type representation
data TyF r
  = TNumber
  | TInt
  | TBool
  | TString
  | TNever
  | TUnknown
  | TLiteral Literal
  | TFunction (List r) r
  | TUnion (List r)
  | TIntersection (List r)
  | -- Polymorphism support
    TGeneric TyGeneric -- Generic type parameter (e.g., T, U)
  | TApplication TyConstructor (List r) -- Type application for Constructor (e.g., Some<T>)

deriving instance (Eq r) => Eq (TyF r)
deriving instance (Ord r) => Ord (TyF r)
deriving instance (Show r) => Show (TyF r)
deriving instance Functor TyF
deriving instance Foldable TyF
deriving instance Traversable TyF

type TyGeneric = Text

type TyConstructor = Text

data Literal
  = LNumber Double
  | LInt Int
  | LBool Bool
  | LString Text

deriving instance Eq Literal
deriving instance Ord Literal
deriving instance Show Literal

newtype TyVarF r = TyVar TyVariable
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type TyVariable = Text

type Ty = Fix (Coproduct '[TyF])

type UnsolvedTy = Fix (Coproduct '[TyVarF, TyF])

tyFormatter :: TyF Text -> Text
tyFormatter = \case
  TNumber -> "number"
  TInt -> "int"
  TBool -> "bool"
  TString -> "string"
  TNever -> "never"
  TUnknown -> "unknown"
  TLiteral (LNumber n) -> T.pack (show n)
  TLiteral (LInt i) -> T.pack (show i)
  TLiteral (LBool b) -> T.pack (show b)
  TLiteral (LString s) -> s
  TFunction args ret ->
    "(fn (" <> T.intercalate ", " args <> ") -> " <> ret <> ")"
  TUnion ts -> "(" <> T.intercalate " | " ts <> ")"
  TIntersection ts -> "(" <> T.intercalate " & " ts <> ")"
  TGeneric t -> t
  TApplication name args ->
    name <> "<" <> T.intercalate ", " args <> ">"

tyVarFormatter :: TyVarF Text -> Text
tyVarFormatter (TyVar v) = "$" <> v

-- | Format an unsolved type
formatUnsolvedTy :: UnsolvedTy -> Text
formatUnsolvedTy = foldFix $ tyVarFormatter ?: tyFormatter ?: absurdVoidF

-- | Format a type
formatTy :: Ty -> Text
formatTy = foldFix $ tyFormatter ?: absurdVoidF

-- | Substitute generics in a type with their corresponding fixed-point types.
-- | This function only takes "Ty" as input because "UnsolvedTy" can contain variables that may be solved to generics. (which we cannot handle here)
substituteGenerics :: (Injective TyF f) => (TyGeneric -> Fix f) -> Ty -> Fix f
substituteGenerics f = foldFix $ (\case
  TGeneric t -> f t
  -- For other types, just return them as is
  other      -> injectFix other
  ) ?: absurdVoidF

typeVars :: UnsolvedTy -> Set TyVariable
typeVars = foldFix $ (\case
  TyVar v -> Set.singleton v
  ) ?: (\case
    other -> fold other
  ) ?: absurdVoidF

-- | TypeScheme represents a type scheme with a list of type variables and a type.
data TypeScheme = TypeScheme (List TyGeneric) Ty
  deriving (Eq, Ord, Show)

formatTypeScheme :: TypeScheme -> Text
formatTypeScheme (TypeScheme vars ty) =
  "(<" <> T.intercalate ", " vars <> "> -> " <> formatTy ty <> ")"

-- | Assign a types to variables in a type scheme.
assignTypeToScheme :: (Injective TyF f) => List (Fix f) -> TypeScheme -> Fix f
assignTypeToScheme assignments (TypeScheme gens ty) =
  let
    substs = Map.fromList $  zip gens assignments
  in
    substituteGenerics
      (\gen -> case Map.lookup gen substs of
        Just t  -> t
        Nothing -> injectFix $ TGeneric gen)
      ty

{-
Transformations
-}

unsolvedTyToTy :: UnsolvedTy -> Maybe Ty
unsolvedTyToTy = foldFixM $ (\case
    TyVar _ -> Nothing
  ) ?: (\case
    other -> Just $ injectFix other
  ) ?: absurdVoidF

tyToUnsolvedTy :: Ty -> UnsolvedTy
tyToUnsolvedTy = foldFix $ (\case
  other      -> injectFix other
  ) ?: absurdVoidF

substituteTyVar :: (Injective TyF f) => (TyVariable -> Fix f) -> UnsolvedTy -> Fix f
substituteTyVar f = foldFix $ (\case
  TyVar v -> f v
  ) ?:  (\case
    other -> injectFix other
  ) ?: absurdVoidF
