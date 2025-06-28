{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.Data.Constraint
  ( Constraint
  , Assumption
  , formatConstraint
  , formatConstraints
  , (?<:)
  , over
  , constraintVars)
  where
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text, intercalate)
import           Language.Memento.Data.Ty (TyVariable, UnsolvedTy,
                                           formatUnsolvedTy, typeVars)

{-
This module defines the data structure for constraints in the type solver.
-}

data Constraint = IsSubtypeOf UnsolvedTy UnsolvedTy
  deriving (Show, Eq, Ord)

type Assumption = Constraint

formatConstraint :: Constraint -> Text
formatConstraint (IsSubtypeOf t1 t2) = formatUnsolvedTy t1 <> " <: " <> formatUnsolvedTy t2

formatConstraints :: Set Constraint -> Text
formatConstraints = intercalate "\n" . Set.toList . Set.map formatConstraint

(?<:) :: UnsolvedTy -> UnsolvedTy -> Constraint
(?<:) = IsSubtypeOf

over :: (UnsolvedTy -> UnsolvedTy) -> Constraint -> Constraint
over f (IsSubtypeOf t1 t2) = IsSubtypeOf (f t1) (f t2)

constraintVars :: Constraint -> Set TyVariable
constraintVars (IsSubtypeOf t1 t2) = Set.union (typeVars t1) (typeVars t2)
