{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.SolveConstraints
  ( solve
  , SolveResult(..)
  , Bounds(..)
  , Substitution
  ) where

{-
This module solves Constraints, and returns a bounds of type variables, or fail.
-}

import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (catMaybes)
import           Data.Set                                     (Set)
import qualified Data.Set                                     as Set
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
import           Language.Memento.Data.Environment.Ty         (TyCons)
import           Language.Memento.Data.Environment.Variance   (Variance (..))
import           Language.Memento.Data.Functor.FixedPoint     (injectFix,
                                                               projectFix)
import           Language.Memento.Data.Ty                     (Ty, TyF (..),
                                                               TyVarF (..),
                                                               TyVariable,
                                                               UnsolvedTy,
                                                               formatUnsolvedTy,
                                                               substituteTyVar,
                                                               tyToUnsolvedTy,
                                                               typeVars,
                                                               unsolvedTyToTy)
import           Language.Memento.TypeSolver.Data.Constraint  (Assumption,
                                                               Constraint (..))
import           Language.Memento.TypeSolver.Normalize        (normalize)
import           Language.Memento.TypeSolver.SolveAssumptions (calculateGenericBounds)
import           Language.Memento.TypeSolver.Subtype          (isSubtype)

-- Data types for constraint solving

data SolveResult
  = Success Substitution
  | Contradiction String
  deriving (Show, Eq)

data Bounds = Bounds [UnsolvedTy] [UnsolvedTy]
  deriving (Show, Eq)

type Substitution = Map TyVariable UnsolvedTy

-- | Main constraint solving function
solve :: TyCons Variance -> Set Assumption -> Set Constraint -> SolveResult
solve varMap assumptions cs =
  let normalized = Set.map normalizeConstraint cs
      (substedAssumptions, substed, substSubst) = substInstancesAsPossible (assumptions, normalized)
      genBndMap = convertGenericBounds $ calculateGenericBounds varMap substedAssumptions
   in case decomposeConstraintsAll varMap genBndMap substed of
        Left err -> Contradiction err
        Right remaining ->
          case branchConstraints varMap substedAssumptions remaining of
            Nothing ->
              -- Only BOUND constraints remain
              let (_, propagatedConstraints, propagationSubst) = calculateFullPropagationAllWithSubst substedAssumptions remaining
              in case checkContradictions varMap genBndMap propagatedConstraints of
                Left err -> Contradiction err
                Right () ->
                  let finalSubstitutions = collectFinalSubstitutions propagatedConstraints
                      combinedSubstitutions = Map.unions [substSubst, propagationSubst, finalSubstitutions]
                  in Success combinedSubstitutions
            Just branches ->
              let branchResults = map (uncurry (solve varMap)) branches
                  successResults = [Map.union substSubst subst | Success subst <- branchResults]
               in case successResults of
                    (combinedSubst:_) -> Success combinedSubst  -- Return first successful substitution combined with accumulated
                    [] -> Contradiction $ "All branches failed: " ++ show branchResults

-- | Repeat decomposition while there are still constraints to decompose
decomposeConstraintsAll :: TyCons Variance -> Map Text (UnsolvedTy, UnsolvedTy) -> Set Constraint -> Either String (Set Constraint)
decomposeConstraintsAll varMap bounds cs = do
  (decomposed, remaining) <- decomposeConstraints varMap bounds cs
  if Set.null remaining
    then Right decomposed
    else do
      nextDecomposed <- decomposeConstraintsAll varMap bounds remaining
      Right (Set.union decomposed nextDecomposed)

-- | Single step constraint decomposition
decomposeConstraints :: TyCons Variance -> Map Text (UnsolvedTy, UnsolvedTy) -> Set Constraint -> Either String (Set Constraint, Set Constraint)
decomposeConstraints varMap bounds cs =
  fmap (\css -> (Set.unions $ map fst css, Set.unions $ map snd css)) $
    mapM (decomposeConstraint varMap bounds) $
      Set.toList cs

-- | Decompose individual constraint and check for clear contradictions
decomposeConstraint :: TyCons Variance -> Map Text (UnsolvedTy, UnsolvedTy) -> Constraint -> Either String (Set Constraint, Set Constraint)
decomposeConstraint varMap bounds = \case
  IsSubtypeOf t1 t2 | t1 == t2 -> Right (Set.empty, Set.empty) -- Same type, no contradiction
  IsSubtypeOf t1 _ | isNever t1 -> Right (Set.empty, Set.empty) -- Never is a subtype of anything
  IsSubtypeOf _ t2 | isUnknown t2 -> Right (Set.empty, Set.empty) -- Unknown is a supertype of anything
  IsSubtypeOf t1 t2 | isGeneric t1 -> Right (Set.singleton (IsSubtypeOf t1 t2), Set.empty)
  IsSubtypeOf t1 t2 | isGeneric t2 -> Right (Set.singleton (IsSubtypeOf t1 t2), Set.empty)
  IsSubtypeOf t1 t2
    | containsNoVars t1 && containsNoVars t2 ->
        if checkSubtype varMap bounds t1 t2
          then Right (Set.empty, Set.empty)
          else Left $ "Contradiction found (while decomposing): (" ++ T.unpack (formatUnsolvedTy t1) ++ ") is not a subtype of (" ++ T.unpack (formatUnsolvedTy t2) ++ ")"
  IsSubtypeOf t1 t2 -> case (projectFix t1, projectFix t2) of
    (Just t1F, Just t2F) -> decomposeByStructure t1F t2F t1 t2
    _                    -> Right (Set.singleton (IsSubtypeOf t1 t2), Set.empty) -- Type variables, leave for branching
  where
    decomposeByStructure t1F t2F t1 t2 = case (t1F, t2F) of
      (TUnion ts1, _) -> Right (Set.empty, Set.fromList [IsSubtypeOf t t2 | t <- ts1])
      (_, TIntersection ts2) -> Right (Set.empty, Set.fromList [IsSubtypeOf t1 t | t <- ts2])
      (TIntersection _, _) -> Right (Set.singleton (IsSubtypeOf t1 t2), Set.empty) -- Leave for branching
      (_, TUnion _) -> Right (Set.singleton (IsSubtypeOf t1 t2), Set.empty) -- Leave for branching
      (TFunction args1 ret1, TFunction args2 ret2)
        | length args1 == length args2 ->
            Right
              ( Set.empty
              , Set.fromList $
                  [IsSubtypeOf a2 a1 | (a1, a2) <- zip args1 args2] -- Contravariant
                    ++ [IsSubtypeOf ret1 ret2] -- Covariant
              )
        | otherwise -> Left $ "Function arity mismatch: " ++ show (length args1) ++ " vs " ++ show (length args2)
      (TApplication name1 args1, TApplication name2 args2)
        | name1 == name2 && length args1 == length args2 -> case Map.lookup name1 varMap of
            Nothing -> Left $ "No variance information for type constructor: " ++ T.unpack name1
            Just (variances, _) ->
              Right
                ( Set.empty
                , Set.fromList
                    [ constraint
                    | (variance, arg1, arg2) <- zip3 variances args1 args2
                    , constraint <- mkConstraintWithVariance arg1 arg2 variance
                    ]
                )
        | otherwise -> Left $ "Type constructor mismatch or arity mismatch: " ++ T.unpack name1 ++ " vs " ++ T.unpack name2
      _ -> Left $ "Contradiction found (error state): (" ++ T.unpack (formatUnsolvedTy t1) ++ ") is not a subtype of (" ++ T.unpack (formatUnsolvedTy t2) ++ "), while decomposing constraints"

-- | Calculate bounds for a type variable
calculateBounds :: TyVariable -> Set Constraint -> Bounds
calculateBounds var cs =
  let lowerBounds = catMaybes [getLowerBound var c | c <- Set.toList cs]
      upperBounds = catMaybes [getUpperBound var c | c <- Set.toList cs]
   in Bounds lowerBounds upperBounds

getLowerBound :: TyVariable -> Constraint -> Maybe UnsolvedTy
getLowerBound var = \case
  IsSubtypeOf t1 t2 | isTyVar var t2 -> Just t1
  _ -> Nothing

getUpperBound :: TyVariable -> Constraint -> Maybe UnsolvedTy
getUpperBound var = \case
  IsSubtypeOf t1 t2 | isTyVar var t1 -> Just t2
  _ -> Nothing

-- | Try to instantiate type variable from its bounds
calculateInstanceFromBounds :: Bounds -> Maybe UnsolvedTy
calculateInstanceFromBounds (Bounds lowers uppers)
  | any isNever uppers = Just tNever
  | any isUnknown lowers = Just tUnknown
  | (g : _) <- filter isGeneric lowers = Just g -- Prefer generic types
  | (g : _) <- filter isGeneric uppers = Just g -- Prefer generic types
  | not (Set.null intersections) = Just (Set.findMin intersections)
  | otherwise = Nothing
 where
  intersections = Set.intersection (Set.fromList lowers) (Set.fromList uppers)

-- | Try to substitute instances as much as possible, returning accumulated substitutions
substInstancesAsPossible :: (Set Assumption, Set Constraint) -> (Set Assumption, Set Constraint, Substitution)
substInstancesAsPossible (as, cs) = substInstancesAsPossibleWithSubst (as, cs) Map.empty

substInstancesAsPossibleWithSubst :: (Set Assumption, Set Constraint) -> Substitution -> (Set Assumption, Set Constraint, Substitution)
substInstancesAsPossibleWithSubst (as, cs) accSubst =
  let csFiltered = filterTrivialConstraints cs
      vars = Set.unions (Set.map constraintVars csFiltered)
      boundsMap = Map.fromList [(var, calculateBounds var csFiltered) | var <- Set.toList vars]
      instances = Map.mapMaybe calculateInstanceFromBounds boundsMap
      -- Filter out self-substitutions and already substituted variables
      validInstances = Map.filterWithKey (\var instType ->
        not (isTyVar var instType) && not (Map.member var accSubst)) instances
   in case Map.lookupMin validInstances of
        Just (var, instanceType) ->
          let subst = Map.singleton var instanceType
              newCs = Set.map (applySubstConstraint subst) csFiltered
              newAs = Set.map (applySubstConstraint subst) as
              newAccSubst = Map.union accSubst subst
           in substInstancesAsPossibleWithSubst (newAs, newCs) newAccSubst
        Nothing ->
          let (finalAs, finalCs, propagationSubst) = calculateFullPropagationAllWithSubst as csFiltered
              combinedSubst = Map.union accSubst propagationSubst
          in (finalAs, finalCs, combinedSubst)

-- | Branch on constraints that require world splitting
branchConstraint :: TyCons Variance -> Constraint -> Maybe [(Substitution, Set Constraint)]
branchConstraint varMap = \case
  IsSubtypeOf t1 t2 -> case (projectFix t1, projectFix t2) of
    (Just (TIntersection ts), _) ->
      Just [(Map.empty, Set.fromList [IsSubtypeOf t t2]) | t <- ts]
    (_, Just (TUnion ts)) ->
      Just [(Map.empty, Set.fromList [IsSubtypeOf t1 t]) | t <- ts]
    _ -> case (isTyVarType t1, isTyVarType t2) of
      (Just var, _) | isFunctionType t2 -> branchTyVarFunction var t2
      (_, Just var) | isFunctionType t1 -> branchFunctionTyVar t1 var
      (Just var, _) | isApplicationType t2 -> branchTyVarApplication varMap var t2
      (_, Just var) | isApplicationType t1 -> branchApplicationTyVar varMap t1 var
      _ -> Nothing

branchTyVarFunction :: TyVariable -> UnsolvedTy -> Maybe [(Substitution, Set Constraint)]
branchTyVarFunction var t2 = case projectFix t2 of
  Just (TFunction args ret) ->
    let argVars = [var <> "_arg_" <> T.pack (show n) | n <- [1 .. length args]]
        retVar = var <> "_ret"
        argVarTypes = [injectFix (TyVar argVar) | argVar <- argVars]
        retVarType = injectFix (TyVar retVar)
        substIfNever = Map.singleton var tNever
        substIfFunc = Map.singleton var (injectFix $ TFunction argVarTypes retVarType)
     in Just
          [ ( substIfFunc
            , Set.fromList [IsSubtypeOf arg argVarType | (argVarType, arg) <- zip argVarTypes args]
                `Set.union` Set.singleton (IsSubtypeOf retVarType ret)
            )
          , (substIfNever, Set.empty)
          ]
  _ -> Nothing

branchFunctionTyVar :: UnsolvedTy -> TyVariable -> Maybe [(Substitution, Set Constraint)]
branchFunctionTyVar t1 var = case projectFix t1 of
  Just (TFunction args ret) ->
    let argVars = [var <> "_arg_" <> T.pack (show n) | n <- [1 .. length args]]
        retVar = var <> "_ret"
        argVarTypes = [injectFix (TyVar argVar) | argVar <- argVars]
        retVarType = injectFix (TyVar retVar)
        substIfUnknown = Map.singleton var tUnknown
        substIfFunc = Map.singleton var (injectFix $ TFunction argVarTypes retVarType)
     in Just
          [ ( substIfFunc
            , Set.fromList [IsSubtypeOf argVarType arg | (argVarType, arg) <- zip argVarTypes args]
                `Set.union` Set.singleton (IsSubtypeOf ret retVarType)
            )
          , (substIfUnknown, Set.empty)
          ]
  _ -> Nothing

branchTyVarApplication :: TyCons Variance -> TyVariable -> UnsolvedTy -> Maybe [(Substitution, Set Constraint)]
branchTyVarApplication varMap var t2 = case projectFix t2 of
  Just (TApplication name args) -> case Map.lookup name varMap of
    Just (variances, _) ->
      let argVars = [var <> "_arg_" <> T.pack (show n) | n <- [1 .. length args]]
          argVarTypes = [injectFix (TyVar argVar) | argVar <- argVars]
          substIfNever = Map.singleton var tNever
          substIfApp = Map.singleton var (injectFix $ TApplication name argVarTypes)
       in Just
            [ ( substIfApp
              , Set.fromList (concat $ zipWith3 mkConstraintWithVariance argVarTypes args variances)
              )
            , (substIfNever, Set.empty)
            ]
    Nothing -> Nothing
  _ -> Nothing

branchApplicationTyVar :: TyCons Variance -> UnsolvedTy -> TyVariable -> Maybe [(Substitution, Set Constraint)]
branchApplicationTyVar varMap t1 var = case projectFix t1 of
  Just (TApplication name args) -> case Map.lookup name varMap of
    Just (variances, _) ->
      let argVars = [var <> "_arg_" <> T.pack (show n) | n <- [1 .. length args]]
          argVarTypes = [injectFix (TyVar argVar) | argVar <- argVars]
          substIfUnknown = Map.singleton var tUnknown
          substIfApp = Map.singleton var (injectFix $ TApplication name argVarTypes)
       in Just
            [ ( substIfApp
              , Set.fromList (concat $ zipWith3 mkConstraintWithVariance args argVarTypes variances)
              )
            , (substIfUnknown, Set.empty)
            ]
    Nothing -> Nothing
  _ -> Nothing

mkConstraintWithVariance :: UnsolvedTy -> UnsolvedTy -> Variance -> [Constraint]
mkConstraintWithVariance left right = \case
  Invariant -> [IsSubtypeOf left right, IsSubtypeOf right left]
  Covariant -> [IsSubtypeOf left right]
  Contravariant -> [IsSubtypeOf right left]
  Bivariant -> []

branchConstraints :: TyCons Variance -> Set Assumption -> Set Constraint -> Maybe [(Set Assumption, Set Constraint)]
branchConstraints varMap as cs = case partitionFirst (branchConstraint varMap) (Set.toList cs) of
  Nothing -> Nothing
  Just (branches, remaining) ->
    Just $
      map
        ( \(subst, cs') ->
            ( Set.map (applySubstConstraint subst) $
                Set.union (Set.fromList remaining) as
            , Set.map (applySubstConstraint subst) $
                Set.union (Set.fromList remaining) cs'
            )
        )
        branches

partitionFirst :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
partitionFirst f xs = go f xs []
 where
  go _ [] _ = Nothing
  go func (x : rest) acc =
    case func x of
      Just b  -> Just (b, acc ++ rest)
      Nothing -> go func rest (x : acc)

-- | Calculate full propagation with substitution tracking
calculateFullPropagation :: Set Assumption -> Set Constraint -> Maybe (Set Constraint, Substitution)
calculateFullPropagation as cns =
  let vars = Set.unions (Set.map constraintVars cns)
      nestedVars = Set.unions (Set.map getNestedVars cns)
      nestedVarsAs = Set.unions (Set.map getNestedVars as)
      nonNestedVars = (vars `Set.difference` nestedVars) `Set.difference` nestedVarsAs
   in if Set.null nonNestedVars
        then Nothing
        else
          let targetVar = case Set.toList nonNestedVars of
                            (v:_) -> v
                            [] -> error "nonNestedVars should not be empty here"
              Bounds lowers uppers = calculateBounds targetVar cns
              newConstraints = [IsSubtypeOf lower upper | lower <- lowers, upper <- uppers]
              oldConstraintsFiltered =
                Set.filter
                  ( \case
                      IsSubtypeOf t1 t2 -> not (containsTyVar targetVar t1 || containsTyVar targetVar t2)
                  )
                  cns
              -- Extract substitution for eliminated variable
              varSubst = case lowers of
                [] -> case uppers of
                  [upper] | containsNoVars upper -> Map.singleton targetVar upper
                  _ -> Map.empty
                [lower] | containsNoVars lower -> Map.singleton targetVar lower
                lowers' | all containsNoVars lowers' -> Map.singleton targetVar (injectFix $ TUnion lowers')
                _ -> Map.empty
           in Just (Set.union oldConstraintsFiltered (Set.fromList newConstraints), varSubst)

calculateFullPropagationAllWithSubst :: Set Assumption -> Set Constraint -> (Set Assumption, Set Constraint, Substitution)
calculateFullPropagationAllWithSubst as cs = calculateFullPropagationAllWithSubstAcc as cs Map.empty

calculateFullPropagationAllWithSubstAcc :: Set Assumption -> Set Constraint -> Substitution -> (Set Assumption, Set Constraint, Substitution)
calculateFullPropagationAllWithSubstAcc as cs accSubst =
  case calculateFullPropagation as cs of
    Nothing -> (as, cs, accSubst)
    Just (newCs, newSubst) ->
      let combinedSubst = Map.union accSubst newSubst
      in calculateFullPropagationAllWithSubstAcc as newCs combinedSubst

{- | Assume that all constraints is BOUND (ref : DECOMPOSE.md)
| propagate bounds
-}
calculatePropagation :: Set Constraint -> Maybe (Set Constraint)
calculatePropagation cs =
  let vars = Set.unions (Set.map constraintVars cs)
      boundsMap = Map.fromList [(var, calculateBounds var cs) | var <- Set.toList vars]
      newConstraints = concat $ Map.elems $ Map.map (\(Bounds lowers uppers) -> [IsSubtypeOf lower upper | lower <- lowers, upper <- uppers]) boundsMap
      actuallyNewConstraints = filter (\c -> not (Set.member c cs) && not (isTrivial c)) newConstraints
   in if null actuallyNewConstraints
        then Nothing
        else Just (Set.fromList actuallyNewConstraints)
 where
  isTrivial (IsSubtypeOf t1 t2) = t1 == t2

-- | Recursive propagation
calculatePropagationAll :: Set Constraint -> Set Constraint
calculatePropagationAll cs =
  case calculatePropagation cs of
    Nothing    -> cs
    Just newCs -> calculatePropagationAll (Set.union cs newCs)

-- | Check for contradictions in final constraints
checkContradictions :: TyCons Variance -> Map Text (UnsolvedTy, UnsolvedTy) -> Set Constraint -> Either String ()
checkContradictions varMap genMap cs = mapM_ check $ Set.toList cs
 where
  check (IsSubtypeOf t1 t2)
    | containsNoVars t1 && containsNoVars t2 =
        if checkSubtype varMap genMap t1 t2
          then Right ()
          else Left $ "Contradiction found (while final check): (" ++ T.unpack (formatUnsolvedTy t1) ++ ") is not a subtype of (" ++ T.unpack (formatUnsolvedTy t2) ++ ")"
  check _ = Right ()

-- | Collect final substitutions from propagated constraints
-- For each type variable x, if we have constraints like T <: x where T contains no variables,
-- we substitute x with the union of all such T's (lower bounds).
-- If x has both lower and upper bounds, we take the lower bound (more specific).
collectFinalSubstitutions :: Set Constraint -> Substitution
collectFinalSubstitutions cs =
  let vars = Set.unions (Set.map constraintVars cs)
      collectSubstForVar var =
        let Bounds lowers uppers = calculateBounds var cs
            -- Filter to only bounds that contain no variables (solved types)
            solvedLowers = [t | t <- lowers, containsNoVars t]
            solvedUppers = [t | t <- uppers, containsNoVars t]
        in case solvedLowers of
             [] -> case solvedUppers of
                     []      -> Nothing  -- No solved bounds, leave unsubstituted
                     [upper] -> Just upper  -- Only upper bound available
                     uppers' -> Just (injectFix $ TIntersection uppers')  -- Multiple upper bounds
             [lower] -> Just lower  -- Single lower bound (preferred)
             lowers' -> Just (injectFix $ TUnion lowers')  -- Multiple lower bounds, union them
  in Map.fromList [(var, subst) | var <- Set.toList vars, Just subst <- [collectSubstForVar var]]

-- Helper functions

-- | Convert generic bounds from Ty to UnsolvedTy
convertGenericBounds :: Map Text (Ty, Ty) -> Map Text (UnsolvedTy, UnsolvedTy)
convertGenericBounds = Map.map (\(lower, upper) -> (tyToUnsolvedTy lower, tyToUnsolvedTy upper))

normalizeConstraint :: Constraint -> Constraint
normalizeConstraint (IsSubtypeOf t1 t2) = IsSubtypeOf (normalize mempty t1) (normalize mempty t2)

filterTrivialConstraints :: Set Constraint -> Set Constraint
filterTrivialConstraints = Set.filter (not . isTrivial)
 where
  isTrivial (IsSubtypeOf t1 t2) = t1 == t2

constraintVars :: Constraint -> Set TyVariable
constraintVars (IsSubtypeOf t1 t2) = Set.union (typeVars t1) (typeVars t2)

applySubstConstraint :: Substitution -> Constraint -> Constraint
applySubstConstraint subst (IsSubtypeOf t1 t2) =
  IsSubtypeOf (applySubstType subst t1) (applySubstType subst t2)

-- | Apply substitution safely, avoiding infinite loops by applying in order
applySubstType :: Substitution -> UnsolvedTy -> UnsolvedTy
applySubstType subst ty =
  let applySubst = \v -> Map.findWithDefault (injectFix (TyVar v)) v subst
  in substituteTyVar applySubst ty

-- Type checking helpers

containsNoVars :: UnsolvedTy -> Bool
containsNoVars ty = case unsolvedTyToTy ty of
  Just _  -> True
  Nothing -> False

checkSubtype :: TyCons Variance -> Map Text (UnsolvedTy, UnsolvedTy) -> UnsolvedTy -> UnsolvedTy -> Bool
checkSubtype varMap genMap t1 t2 = case (unsolvedTyToTy t1, unsolvedTyToTy t2) of
  (Just ty1, Just ty2) ->
    let convertedGenMap = Map.mapMaybe (\(lower, upper) ->
          case (unsolvedTyToTy lower, unsolvedTyToTy upper) of
            (Just lowerTy, Just upperTy) -> Just (lowerTy, upperTy)
            _                            -> Nothing) genMap
    in case isSubtype varMap convertedGenMap ty1 ty2 of
         Right result -> result
         Left _       -> False
  _ -> False

isNever :: UnsolvedTy -> Bool
isNever ty = case projectFix ty of
  Just TNever -> True
  _           -> False

isUnknown :: UnsolvedTy -> Bool
isUnknown ty = case projectFix ty of
  Just TUnknown -> True
  _             -> False

isGeneric :: UnsolvedTy -> Bool
isGeneric ty = case projectFix ty of
  Just (TGeneric _) -> True
  _                 -> False

isFunctionType :: UnsolvedTy -> Bool
isFunctionType ty = case projectFix ty of
  Just (TFunction _ _) -> True
  _                    -> False

isApplicationType :: UnsolvedTy -> Bool
isApplicationType ty = case projectFix ty of
  Just (TApplication _ _) -> True
  _                       -> False

isTyVarType :: UnsolvedTy -> Maybe TyVariable
isTyVarType ty = case projectFix ty of
  Just (TyVar var) -> Just var
  Nothing          -> Nothing

isTyVar :: TyVariable -> UnsolvedTy -> Bool
isTyVar var ty = case isTyVarType ty of
  Just v  -> v == var
  Nothing -> False

containsTyVar :: TyVariable -> UnsolvedTy -> Bool
containsTyVar var ty = Set.member var (typeVars ty)

getNestedVars :: Constraint -> Set TyVariable
getNestedVars (IsSubtypeOf t1 t2) = case (isTyVarType t1, isTyVarType t2) of
  (Just _, Just _) -> Set.empty -- Both are type variables, no nesting
  (Just _, _)      -> typeVars t2 -- t2 contains nested variables
  (_, Just _)      -> typeVars t1 -- t1 contains nested variables
  _                -> Set.union (typeVars t1) (typeVars t2) -- Both contain nested variables

-- Create basic types
tNever :: UnsolvedTy
tNever = injectFix TNever

tUnknown :: UnsolvedTy
tUnknown = injectFix TUnknown
