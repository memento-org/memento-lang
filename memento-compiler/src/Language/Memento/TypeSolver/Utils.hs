{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Language.Memento.TypeSolver.Utils
  ( -- Type checking predicates
    isNever
  , isUnknown
  , isGeneric
  , containsGeneric
  , isTrivialConstraint
    -- Type structure predicates
  , isFunctionType
  , isApplicationType
  , isTyVarType
  , isTyVar
  , containsTyVar
    -- Basic type constructors
  , tNeverUnsolved
  , tUnknownUnsolved
  , tNever
  , tUnknown
    -- Shared data types
  , Bounds(..)
    -- Shared constraint generation
  , boundsToConstraints
  , mkConstraintWithVariance
    -- General utilities
  , partitionFirst
  , getNestedVars
  ) where

import           Data.Set                                    (Set)
import qualified Data.Set                                    as Set
import           Language.Memento.Data.Environment.Variance  (Variance (..))
import           Language.Memento.Data.Functor.Coproduct     (Injective)
import           Language.Memento.Data.Functor.FixedPoint    (Fix, injectFix,
                                                              projectFix)
import           Language.Memento.Data.Ty                    (Ty, TyF (..),
                                                              TyVarF (..),
                                                              TyVariable,
                                                              UnsolvedTy,
                                                              typeVars)
import           Language.Memento.TypeSolver.Data.Constraint (Constraint (..))

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

-- | Check if a type is a generic
isGeneric :: (Injective TyF t) => Fix t -> Bool
isGeneric ty = case projectFix ty of
  Just (TGeneric _) -> True
  _                 -> False

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
  Nothing -> True

-- | Create a TNever type (UnsolvedTy version)
tNeverUnsolved :: UnsolvedTy
tNeverUnsolved = injectFix TNever

-- | Create a TUnknown type (UnsolvedTy version)
tUnknownUnsolved :: UnsolvedTy
tUnknownUnsolved = injectFix TUnknown

-- | Create a TNever type (Ty version)
tNever :: Ty
tNever = injectFix TNever

-- | Create a TUnknown type (Ty version)
tUnknown :: Ty
tUnknown = injectFix TUnknown

-- | Check if a constraint is trivial (t1 <: t1)
isTrivialConstraint :: Constraint -> Bool
isTrivialConstraint (IsSubtypeOf t1 t2) = t1 == t2

-- | Bounds for a variable (lower and upper bounds)
data Bounds = Bounds [UnsolvedTy] [UnsolvedTy]
  deriving (Show, Eq)

-- | Convert bounds to constraints (identical pattern from both files)
boundsToConstraints :: Bounds -> [Constraint]
boundsToConstraints (Bounds lowers uppers) = [IsSubtypeOf lower upper | lower <- lowers, upper <- uppers]

-- | Create constraints based on variance (extracted from both files)
mkConstraintWithVariance :: UnsolvedTy -> UnsolvedTy -> Variance -> [Constraint]
mkConstraintWithVariance left right = \case
  Invariant -> [IsSubtypeOf left right, IsSubtypeOf right left]
  Covariant -> [IsSubtypeOf left right]
  Contravariant -> [IsSubtypeOf right left]
  Bivariant -> []

-- | Partition list by finding first matching element (general utility)
partitionFirst :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
partitionFirst f xs = go f xs []
 where
  go _ [] _ = Nothing
  go func (x : rest) acc =
    case func x of
      Just b  -> Just (b, acc ++ rest)
      Nothing -> go func rest (x : acc)

-- Type structure predicates

-- | Check if type is a function type
isFunctionType :: UnsolvedTy -> Bool
isFunctionType ty = case projectFix ty of
  Just (TFunction _ _) -> True
  _                    -> False

-- | Check if type is a type application
isApplicationType :: UnsolvedTy -> Bool
isApplicationType ty = case projectFix ty of
  Just (TApplication _ _) -> True
  _                       -> False

-- | Extract type variable from UnsolvedTy if it is one
isTyVarType :: UnsolvedTy -> Maybe TyVariable
isTyVarType ty = case projectFix ty of
  Just (TyVar var) -> Just var
  Nothing          -> Nothing

-- | Check if UnsolvedTy is a specific type variable
isTyVar :: TyVariable -> UnsolvedTy -> Bool
isTyVar var ty = case isTyVarType ty of
  Just v  -> v == var
  Nothing -> False

-- | Check if type contains a specific type variable
containsTyVar :: TyVariable -> UnsolvedTy -> Bool
containsTyVar var ty = Set.member var (typeVars ty)

-- | Get nested type variables from constraint
getNestedVars :: Constraint -> Set TyVariable
getNestedVars (IsSubtypeOf t1 t2) = case (isTyVarType t1, isTyVarType t2) of
  (Just _, Just _) -> Set.empty -- Both are type variables, no nesting
  (Just _, _)      -> typeVars t2 -- t2 contains nested variables
  (_, Just _)      -> typeVars t1 -- t1 contains nested variables
  _                -> Set.union (typeVars t1) (typeVars t2) -- Both contain nested variables
