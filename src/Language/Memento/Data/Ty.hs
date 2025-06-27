{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Language.Memento.Data.Ty (TyF (..), VarF (..), Ty, UnsolvedTy, tyFormatter, varFormatter, formatUnsolvedTy, formatTy) where

import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           GHC.Base                                 (List)
import           Language.Memento.Data.Functor.Coproduct  (Coproduct,
                                                           absurdVoidF, (?:))
import           Language.Memento.Data.Functor.FixedPoint (Fix, foldFix)

-- Type representation
data TyF r
  = TNumber
  | TInt
  | TBool
  | TString
  | TNever
  | TUnknown
  | TLiteral Literal
  | TFunction (List (Text, r)) r
  | TUnion (List r)
  | TIntersection (List r)
  | -- Polymorphism support
    TGeneric Text -- Generic type parameter (e.g., T, U)
  | TApplication Text (List r) -- Type application for Constructor (e.g., Some<T>)

deriving instance (Eq r) => Eq (TyF r)
deriving instance (Ord r) => Ord (TyF r)
deriving instance (Show r) => Show (TyF r)
deriving instance Functor TyF
deriving instance Foldable TyF
deriving instance Traversable TyF

data Literal
  = LNumber Double
  | LInt Int
  | LBool Bool
  | LString Text

deriving instance Eq Literal
deriving instance Ord Literal
deriving instance Show Literal

newtype VarF r = TyVar Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Ty = Fix TyF

type UnsolvedTy = Fix (Coproduct '[TyF, VarF])

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
    "(fn " <> T.intercalate ", " (map (\(n, t) -> n <> ": " <> t) args) <> " -> " <> ret <> ")"
  TUnion ts -> "(" <> T.intercalate " | " ts <> ")"
  TIntersection ts -> "(" <> T.intercalate " & " ts <> ")"
  TGeneric t -> t
  TApplication name args ->
    name <> "<" <> T.intercalate ", " args <> ">"

varFormatter :: VarF Text -> Text
varFormatter (TyVar v) = "$" <> v

-- | Format an unsolved type
formatUnsolvedTy :: UnsolvedTy -> Text
formatUnsolvedTy = foldFix $ tyFormatter ?: varFormatter ?: absurdVoidF

-- | Format a type
formatTy :: Ty -> Text
formatTy = foldFix tyFormatter

-- | TypeScheme represents a type scheme with a list of type variables and a type.
data TypeScheme t = TypeScheme (List Text) t
  deriving (Eq, Ord, Show)
