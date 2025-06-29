{-# LANGUAGE LambdaCase #-}

module Language.Memento.TypeSolver.Constraint.Utils
  ( -- Constraint operations
    constraintVars
  , filterTrivialConstraints
  , normalizeConstraint
    -- Substitution operations
  , applySubstConstraint
  , applySubstType
    -- Type variable helpers
  , containsNoVars
  ) where

import           Data.Set                                      (Set)
import qualified Data.Set                                      as Set
import           Data.Map                                      (Map)
import qualified Data.Map                                      as Map
import           Language.Memento.Data.Ty                      (TyVariable, UnsolvedTy, 
                                                                substituteTyVar, typeVars,
                                                                unsolvedTyToTy)
import           Language.Memento.TypeSolver.Data.Constraint   (Constraint (..))
import           Language.Memento.TypeSolver.Normalize         (normalize)
import           Language.Memento.TypeSolver.Utils            (isTrivialConstraint)
import           Language.Memento.Data.Functor.FixedPoint      (injectFix)
import           Language.Memento.Data.Ty                      (TyVarF (..))

type Substitution = Map TyVariable UnsolvedTy

-- | Extract all type variables from constraint
constraintVars :: Constraint -> Set TyVariable
constraintVars (IsSubtypeOf t1 t2) = Set.union (typeVars t1) (typeVars t2)

-- | Filter out trivial constraints
filterTrivialConstraints :: Set Constraint -> Set Constraint
filterTrivialConstraints = Set.filter (not . isTrivialConstraint)

-- | Normalize a constraint
normalizeConstraint :: Constraint -> Constraint
normalizeConstraint (IsSubtypeOf t1 t2) = IsSubtypeOf (normalize mempty t1) (normalize mempty t2)

-- | Apply substitution to constraint
applySubstConstraint :: Substitution -> Constraint -> Constraint
applySubstConstraint subst (IsSubtypeOf t1 t2) =
  IsSubtypeOf (applySubstType subst t1) (applySubstType subst t2)

-- | Apply substitution safely, avoiding infinite loops by applying in order
applySubstType :: Substitution -> UnsolvedTy -> UnsolvedTy
applySubstType subst ty =
  let applySubst = \v -> Map.findWithDefault (injectFix (TyVar v)) v subst
  in substituteTyVar applySubst ty

-- | Check if type contains no type variables
containsNoVars :: UnsolvedTy -> Bool
containsNoVars ty = case unsolvedTyToTy ty of
  Just _  -> True
  Nothing -> False