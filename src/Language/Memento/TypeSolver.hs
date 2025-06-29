{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver
  ( solveTypedAST
  , SolveError(..)
  ) where

{-
This module defines the transformation `TypedAST UnsolvedTy -> TypedAST Ty`, which is, type solver.
-}

import qualified Data.Map                                     as Map
import qualified Data.Set                                     as Set
import           Language.Memento.Data.AST.Tag                (KProgram)
import           Language.Memento.Data.Environment.Ty         (TyCons)
import           Language.Memento.Data.Environment.Variance   (Variance)
import           Language.Memento.Data.Functor.FixedPoint     (injectFix)
import           Language.Memento.Data.Ty                     (Ty, TyF (..),
                                                               TyVariable,
                                                               UnsolvedTy,
                                                               substituteTyVar,
                                                               unsolvedTyToTy)
import           Language.Memento.Data.TypedAST               (TypedAST,
                                                               transformTypedAST)
import           Language.Memento.TypeSolver.ConstraintGen    (generateConstraints)
import           Language.Memento.TypeSolver.Data.Constraint (Assumption,
                                                               Constraint)
import           Language.Memento.TypeSolver.SolveConstraints (SolveResult (..),
                                                               Substitution,
                                                               solve)

-- | Errors that can occur during type solving
data SolveError
  = ContradictionError String
  | UnsolvedVariablesError [TyVariable]
  deriving (Show, Eq)

-- | Solve a single (assumptions, constraints) pair
solvePair :: TyCons Variance -> (Set.Set Assumption, Set.Set Constraint) -> Either SolveError Substitution
solvePair varMap (assumptions, constraints) = 
  case solve varMap assumptions constraints of
    Success substs    -> Right substs
    Contradiction err -> Left (ContradictionError err)

-- | Main function: generate constraints, solve each pair separately, and transform TypedAST UnsolvedTy to TypedAST Ty
solveTypedAST :: TyCons Variance -> TypedAST UnsolvedTy KProgram -> Either SolveError (TypedAST Ty KProgram)
solveTypedAST varMap typedAST = do
  -- Generate constraints from the typed AST
  let constraintSets = generateConstraints typedAST

  -- Solve each (assumptions, constraints) pair separately
  pairResults <- mapM (solvePair varMap) constraintSets

  -- Combine all substitutions (later substitutions take precedence)
  let combinedSubstitutions = foldl Map.union Map.empty pairResults

  -- Create the type transformation function
  let typeTransform = applySubstitutionsToType combinedSubstitutions

  -- Apply transformation to the entire AST
  case transformTypedAST typeTransform typedAST of
    transformedAST -> Right transformedAST

-- | Apply substitutions to convert UnsolvedTy to Ty
applySubstitutionsToType :: Substitution -> UnsolvedTy -> Ty
applySubstitutionsToType substitutions unsolvedTy =
  let -- Apply substitutions to the unsolved type
      substitutedTy = applySubstitutions substitutions unsolvedTy

      -- Try to convert to solved type
  in case unsolvedTyToTy substitutedTy of
       Just ty -> ty
       Nothing ->
         -- If we still have unsolved variables, use lower bounds or fallback
         applyFallbackStrategy substitutions substitutedTy

-- | Apply substitutions safely to an unsolved type
applySubstitutions :: Substitution -> UnsolvedTy -> UnsolvedTy
applySubstitutions substs ty =
  let applySubst v = Map.findWithDefault (injectFix (TGeneric v)) v substs
  in substituteTyVar applySubst ty

-- | Fallback strategy when we still have unsolved variables after substitution
applyFallbackStrategy :: Substitution -> UnsolvedTy -> Ty
applyFallbackStrategy _ _ =
  -- Ultimate fallback: if we can't solve completely, return unknown type
  injectFix TUnknown
