{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.SolveAssumptions
  ( calculateGenericBounds
  , decomposeAssumptionAll
  , decomposeAssumption
  ) where

{-
This module defines "assumption algorithms" to decompose assumptions to generic bounds.
-}

import           Data.Map                                    (Map)
import qualified Data.Map                                    as Map
import           Data.Maybe                                  (mapMaybe)
import           Data.Set                                    (Set)
import qualified Data.Set                                    as Set
import           Data.Text                                   (Text)
import           Language.Memento.Data.Environment.Ty        (TyCons)
import           Language.Memento.Data.Environment.Variance  (Variance (..))
import           Language.Memento.Data.Functor.FixedPoint    (injectFix,
                                                              projectFix)
import           Language.Memento.Data.Ty                    (Ty, TyF (..),
                                                              UnsolvedTy,
                                                              unsolvedTyToTy)
import           Language.Memento.TypeSolver.Data.Constraint (Assumption,
                                                              Constraint (..))

-- | Calculate generic bounds from a set of assumptions
calculateGenericBounds :: TyCons Variance -> Set Assumption -> Map Text (Ty, Ty)
calculateGenericBounds _varMap assumptions =
  let
    takeUpper :: Text -> Constraint -> Maybe Ty
    takeUpper name = \case
      IsSubtypeOf t1 t2 | isGenericNamed name t1 -> unsolvedTyToTy t2
      _ -> Nothing

    takeLower :: Text -> Constraint -> Maybe Ty
    takeLower name = \case
      IsSubtypeOf t1 t2 | isGenericNamed name t2 -> unsolvedTyToTy t1
      _ -> Nothing

    takeUppers name = mapMaybe (takeUpper name) $ Set.toList assumptions
    takeLowers name = mapMaybe (takeLower name) $ Set.toList assumptions

    generics = mapMaybe extractGenericName $ Set.toList assumptions

    extractGenericName = \case
      IsSubtypeOf t1 t2 -> case (extractGeneric t1, extractGeneric t2) of
        (Just n, _) -> Just n
        (_, Just n) -> Just n
        _           -> Nothing
   in
    Map.fromList $
      map
        ( \name ->
            let uppers = takeUppers name
                lowers = takeLowers name
                upperBound = case uppers of
                  []  -> tUnknown
                  [t] -> t
                  ts  -> injectFix $ TIntersection ts
                lowerBound = case lowers of
                  []  -> tNever
                  [t] -> t
                  ts  -> injectFix $ TUnion ts
             in (name, (lowerBound, upperBound))
        )
        generics

-- | Recursively decompose all assumptions until a fixpoint is reached
decomposeAssumptionAll :: TyCons Variance -> Set Assumption -> Set Assumption
decomposeAssumptionAll varMap assumptions =
  let decomposeResult = Set.map (decomposeAssumption varMap) assumptions
      remained = Set.unions $ Set.map fst decomposeResult
      decomposed = Set.unions $ Set.map snd decomposeResult
   in if Set.null decomposed
        then remained
        else Set.union remained $ decomposeAssumptionAll varMap decomposed

-- | Decomposes a assumption into assumptions.
-- Returns (remained for later generics bound computation, decomposed)
-- Includes constraint propagation for better inference
decomposeAssumption :: TyCons Variance -> Constraint -> (Set Assumption, Set Assumption)
decomposeAssumption varMap = \case
  IsSubtypeOf t1 t2 | t1 == t2 -> (Set.empty, Set.empty) -- Same type, no contradiction
  IsSubtypeOf t1 _ | isNever t1 -> (Set.empty, Set.empty) -- Never is a subtype of anything
  IsSubtypeOf _ t2 | isUnknown t2 -> (Set.empty, Set.empty) -- Unknown is a supertype of anything
  IsSubtypeOf t1 t2
    | not (containsGeneric t1 || containsGeneric t2) -> (Set.empty, Set.empty)
    | otherwise -> case (projectFix t1, projectFix t2) of
        (Just t1F, Just t2F) -> decomposeByStructure t1F t2F t1 t2
        _                    -> (Set.empty, Set.empty)
  where
    decomposeByStructure t1F t2F t1 t2 = case (t1F, t2F) of
      -- Union/Intersection decomposition
      (TUnion ts, _) -> (Set.empty, Set.fromList [IsSubtypeOf t' t2 | t' <- ts])
      (_, TIntersection ts) -> (Set.empty, Set.fromList [IsSubtypeOf t1 t' | t' <- ts])
      (_, TUnion _) -> (Set.empty, Set.empty) -- Cannot decompose t <: Union
      (TIntersection _, _) -> (Set.empty, Set.empty) -- Cannot decompose Intersection <: t

      -- Save generic constraints for later bound computation
      (TGeneric _, _) -> (Set.singleton $ IsSubtypeOf t1 t2, Set.empty)
      (_, TGeneric _) -> (Set.singleton $ IsSubtypeOf t1 t2, Set.empty)

      -- Decompose structured types
      (TFunction args1 ret1, TFunction args2 ret2)
        | length args1 == length args2 ->
            let argAssumptions = Set.fromList $ zipWith IsSubtypeOf args2 args1 -- Contravariant
                retAssumptions = Set.singleton $ IsSubtypeOf ret1 ret2 -- Covariant
             in (Set.empty, argAssumptions `Set.union` retAssumptions)
        | otherwise -> (Set.empty, Set.empty)

      (TApplication tc1 args1, TApplication tc2 args2)
        | tc1 == tc2
        , Just (variances, _) <- Map.lookup tc1 varMap
        , length args1 == length args2 ->
            let argAssumptions = Set.unions $ zipWith3 mkAssumption variances args1 args2
                mkAssumption variance arg1 arg2 =
                  case variance of
                    Covariant -> Set.singleton $ IsSubtypeOf arg1 arg2
                    Contravariant -> Set.singleton $ IsSubtypeOf arg2 arg1
                    Invariant -> Set.fromList [IsSubtypeOf arg1 arg2, IsSubtypeOf arg2 arg1]
                    Bivariant -> Set.empty
             in (Set.empty, argAssumptions)
        | otherwise -> (Set.empty, Set.empty)

      _ -> (Set.empty, Set.empty) -- Default case, keep as is

-- Helper functions

-- | Check if a type is a generic with the given name
isGenericNamed :: Text -> UnsolvedTy -> Bool
isGenericNamed name ty = case extractGeneric ty of
  Just n  -> n == name
  Nothing -> False

-- | Extract generic name from a type if it's a generic
extractGeneric :: UnsolvedTy -> Maybe Text
extractGeneric ty = case projectFix ty of
  Just (TGeneric name) -> Just name
  _                    -> Nothing

-- | Check if a type contains any generics
containsGeneric :: UnsolvedTy -> Bool
containsGeneric ty = case projectFix ty of
  Just tyF -> case tyF of
    TGeneric _          -> True
    TFunction args ret  -> any containsGeneric args || containsGeneric ret
    TApplication _ args -> any containsGeneric args
    TUnion ts           -> any containsGeneric ts
    TIntersection ts    -> any containsGeneric ts
    _                   -> False
  Nothing -> False -- TyVar case - no generics in type variables

-- | Check if a type is TNever
isNever :: UnsolvedTy -> Bool
isNever ty = case projectFix ty of
  Just TNever -> True
  _           -> False

-- | Check if a type is TUnknown
isUnknown :: UnsolvedTy -> Bool
isUnknown ty = case projectFix ty of
  Just TUnknown -> True
  _             -> False


-- | Create a TNever type
tNever :: Ty
tNever = injectFix TNever

-- | Create a TUnknown type
tUnknown :: Ty
tUnknown = injectFix TUnknown
