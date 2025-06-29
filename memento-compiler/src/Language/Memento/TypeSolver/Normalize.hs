{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.Normalize
  ( normalize
  , normalizeStep
  ) where

{-
This module defines the normalization of types.
Normalization simplifies union and intersection types without subtyping checks.
Works with UnsolvedTy containing type variables.
-}

import qualified Data.Set                                   as Set
import           Language.Memento.Data.Environment.Ty       (TyCons)
import           Language.Memento.Data.Environment.Variance (Variance)
import           Language.Memento.Data.Functor.FixedPoint   (injectFix,
                                                             projectFix)
import           Language.Memento.Data.Ty                   (TyF (..),
                                                             UnsolvedTy)

-- | Normalize an unsolved type (containing type variables)
-- Works directly with types containing variables
normalize :: TyCons Variance -> UnsolvedTy -> UnsolvedTy
normalize variances = fixpoint (normalizeStep variances)
  where
    fixpoint f x =
      let x' = f x
       in if x == x' then x else fixpoint f x'

-- | Single normalization step for unsolved types
normalizeStep :: TyCons Variance -> UnsolvedTy -> UnsolvedTy
normalizeStep variances ty = case projectFix ty of
  Just tyF -> case tyF of
    TNumber -> ty
    TInt -> ty
    TBool -> ty
    TString -> ty
    TNever -> ty
    TUnknown -> ty
    TLiteral _ -> ty
    TGeneric _ -> ty
    TFunction args ret ->
      injectFix $ TFunction (map (normalize variances) args) (normalize variances ret)
    TUnion ts -> normalizeUnion $ map (normalize variances) ts
    TIntersection ts -> normalizeIntersection $ map (normalize variances) ts
    TApplication name args ->
      injectFix $ TApplication name (map (normalize variances) args)
  Nothing -> ty -- TyVar case - leave as is

-- | Normalize union types
normalizeUnion :: [UnsolvedTy] -> UnsolvedTy
normalizeUnion ts =
  let
    -- Remove TNever from unions (A | never = A)
    withoutNever = filter (not . isNever) ts
    -- Check for TUnknown (A | unknown = unknown)
    hasUnknown = any isUnknown ts
    -- Flatten nested unions
    flattened = concatMap flattenUnion withoutNever
    -- Remove duplicates by converting to Set and back
    deduplicated = Set.toList $ Set.fromList flattened
   in case (hasUnknown, deduplicated) of
        (True, _)    -> tUnknown
        (False, [])  -> tNever
        (False, [t]) -> t
        (False, ts') -> injectFix $ TUnion ts'

-- | Normalize intersection types
normalizeIntersection :: [UnsolvedTy] -> UnsolvedTy
normalizeIntersection ts =
  let
    -- Remove TUnknown from intersections (A & unknown = A)
    withoutUnknown = filter (not . isUnknown) ts
    -- Check for TNever (A & never = never)
    hasNever = any isNever ts
    -- Flatten nested intersections
    flattened = concatMap flattenIntersection withoutUnknown
    -- Remove duplicates by converting to Set and back
    deduplicated = Set.toList $ Set.fromList flattened
   in case (hasNever, deduplicated) of
        (True, _)    -> tNever
        (False, [])  -> tUnknown
        (False, [t]) -> t
        (False, ts') -> injectFix $ TIntersection ts'

-- | Flatten union types recursively
flattenUnion :: UnsolvedTy -> [UnsolvedTy]
flattenUnion ty = case projectFix ty of
  Just (TUnion ts) -> concatMap flattenUnion ts
  _                -> [ty]

-- | Flatten intersection types recursively
flattenIntersection :: UnsolvedTy -> [UnsolvedTy]
flattenIntersection ty = case projectFix ty of
  Just (TIntersection ts) -> concatMap flattenIntersection ts
  _                       -> [ty]

-- Helper functions

-- | Check if an unsolved type is TNever
isNever :: UnsolvedTy -> Bool
isNever ty = case projectFix ty of
  Just TNever -> True
  _           -> False

-- | Check if an unsolved type is TUnknown
isUnknown :: UnsolvedTy -> Bool
isUnknown ty = case projectFix ty of
  Just TUnknown -> True
  _             -> False

-- | Create a TNever unsolved type
tNever :: UnsolvedTy
tNever = injectFix TNever

-- | Create a TUnknown unsolved type
tUnknown :: UnsolvedTy
tUnknown = injectFix TUnknown