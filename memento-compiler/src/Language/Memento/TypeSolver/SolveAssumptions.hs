{-# LANGUAGE LambdaCase #-}

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
                                                              TyVarF (..),
                                                              TyVariable,
                                                              UnsolvedTy,
                                                              typeVars,
                                                              unsolvedTyToTy)
import           Language.Memento.TypeSolver.Data.Constraint (Assumption,
                                                              Constraint (..))
import           Language.Memento.TypeSolver.Utils           (Bounds (..),
                                                              boundsToConstraints,
                                                              containsGeneric,
                                                              isNever,
                                                              isTrivialConstraint,
                                                              isUnknown)

-- | Calculate generic bounds from a set of assumptions
calculateGenericBounds :: TyCons Variance -> Set Assumption -> Map Text (Set Ty, Set Ty)
calculateGenericBounds varMap assumptions =
  let
    -- Apply propagation first to find all transitive relationships
    propagatedAssumptions = calculatePropagationAssumptionsAll $ decomposeAssumptionAll varMap assumptions
    takeUpper :: Text -> Constraint -> Maybe Ty
    takeUpper name = \case
      IsSubtypeOf t1 t2 | isGenericNamed name t1 -> unsolvedTyToTy t2
      _ -> Nothing

    takeLower :: Text -> Constraint -> Maybe Ty
    takeLower name = \case
      IsSubtypeOf t1 t2 | isGenericNamed name t2 -> unsolvedTyToTy t1
      _ -> Nothing

    takeUppers name = mapMaybe (takeUpper name) $ Set.toList propagatedAssumptions
    takeLowers name = mapMaybe (takeLower name) $ Set.toList propagatedAssumptions

    generics = mapMaybe extractGenericName $ Set.toList propagatedAssumptions

    extractGenericName = \case
      IsSubtypeOf t1 t2 -> case (extractGeneric t1, extractGeneric t2) of
        (Just n, _) -> Just n
        (_, Just n) -> Just n
        _           -> Nothing

    result = Map.fromList $
      map
        ( \name ->
            let uppers = takeUppers name
                lowers = takeLowers name
             in (name, (Set.fromList lowers, Set.fromList uppers))
        )
        generics
   in -- trace (T.unpack $ "\nFINAL PROPAGATED GEN MAP\n" <> T.pack (show result)) $
    result

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
        _                    -> (Set.singleton $ IsSubtypeOf t1 t2, Set.empty)
  where
    decomposeByStructure t1F t2F t1 t2 = case (t1F, t2F) of
      -- Union/Intersection decomposition
      (TUnion ts, _) -> (Set.empty, Set.fromList [IsSubtypeOf t' t2 | t' <- ts])
      (_, TIntersection ts) -> (Set.empty, Set.fromList [IsSubtypeOf t1 t' | t' <- ts])

      (_, TUnion []) -> (Set.empty, Set.singleton $ IsSubtypeOf (injectFix t1F) $ injectFix TNever) -- Empty union, t <: Never
      (TIntersection [], _) -> (Set.empty, Set.singleton $ IsSubtypeOf (injectFix TUnknown) (injectFix t2F)) -- Empty intersection, Unknown <: t2

      (tl, TUnion [tr]) -> (Set.empty, Set.singleton $ IsSubtypeOf (injectFix tl) tr) -- Empty union, t <: Never
      (TIntersection [tl], tr) -> (Set.empty, Set.singleton $ IsSubtypeOf tl (injectFix tr)) -- Empty intersection, Unknown <: t2

      -- 構造判定
      -- 右辺の TUnion が Unknown を含まない場合、左辺と同じ TApplication を残して右辺を削れる (異なる data types 間に関係が無いので)
      -- またその時に、バリアンスに基づいて、バリアンスが正なら | として結合し右側、負なら & として結合し左側にできる
      (TApplication tc1 args1, TUnion trs)
        | not (any isUnknown trs)
        , Just (variances, _) <- Map.lookup tc1 varMap
        -> let
            filteredTrsArgs = mapMaybe (\t -> case projectFix t of
              Just (TApplication tc2 args2) | tc1 == tc2 && length args2 == length args1 ->
                Just args2
              _ -> Nothing) trs
            -- 第 idx 引数に対応した assumptions を取ってくる
            argAssumptions idx =
              let
                arg2s = map (!! idx) filteredTrsArgs
                variance = variances !! idx
                arg1 = args1 !! idx
              in case variance of
                -- Covariant 右辺に args の union を持ってくる
                Covariant -> Set.singleton $ IsSubtypeOf arg1 (injectFix $ TUnion arg2s)
                -- Contravariant 左辺に args の intersection を持ってくる
                Contravariant -> Set.singleton $ IsSubtypeOf (injectFix $ TIntersection arg2s) arg1
                -- Invariant, Bivariant の場合はとりあえず情報なしとしておく
                _ -> Set.empty
           in -- 各引数の assumptions を結合して返す
            (Set.empty, Set.unions $ map argAssumptions [0 .. length args1 - 1])

      -- 同様。左辺の TIntersection が Never を含まない場合、右辺と同じ TApplication を残して左辺を削れる
      -- またその時に、バリアンスが正なら & として左側、負なら | として結合し右側にできる
      (TIntersection tls, TApplication tc2 args2)
        | not (any isNever tls)
        , Just (variances, _) <- Map.lookup tc2 varMap
        -> let
            filteredTlsArgs = mapMaybe (\t -> case projectFix t of
              Just (TApplication tc1 args1) | tc1 == tc2 && length args1 == length args2 ->
                Just args1
              _ -> Nothing) tls
            -- 第 idx 引数に対応した assumptions を取ってくる
            argAssumptions idx =
              let
                arg1s = map (!! idx) filteredTlsArgs
                variance = variances !! idx
                arg2 = args2 !! idx
              in case variance of
                -- Covariant 左辺に args の intersection を持ってくる
                Covariant -> Set.singleton $ IsSubtypeOf (injectFix $ TIntersection arg1s) arg2
                -- Contravariant 右辺に args の union を持ってくる
                Contravariant -> Set.singleton $ IsSubtypeOf arg2 (injectFix $ TUnion arg1s)
                -- Invariant, Bivariant の場合はとりあえず情報なしとしておく
                _ -> Set.empty
           in -- 各引数の assumptions を結合して返す
            (Set.empty, Set.unions $ map argAssumptions [0 .. length args2 - 1])

      (_, TUnion _) -> (Set.empty, Set.empty)
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


-- Propagation functionality

-- | Unified variable type to handle both generics and type variables
data Variable = GenericVar Text | TypeVar TyVariable
  deriving (Eq, Ord, Show)

-- | Extract all variables (both generic and type vars) from assumptions
extractAllVars :: Set Assumption -> Set Variable
extractAllVars assumptions = Set.unions $ Set.map extractVarsFromConstraint assumptions
  where
    extractVarsFromConstraint (IsSubtypeOf t1 t2) =
      Set.union (extractVarsFromType t1) (extractVarsFromType t2)

    extractVarsFromType :: UnsolvedTy -> Set Variable
    extractVarsFromType ty =
      let typeVarSet = Set.map TypeVar $ typeVars ty
          genericSet = extractGenericsFromType ty
      in Set.union typeVarSet genericSet

    extractGenericsFromType :: UnsolvedTy -> Set Variable
    extractGenericsFromType ty = case projectFix ty of
      Just tyF -> case tyF of
        TGeneric name       -> Set.singleton (GenericVar name)
        TFunction args ret  -> Set.unions $ extractGenericsFromType ret : map extractGenericsFromType args
        TApplication _ args -> Set.unions $ map extractGenericsFromType args
        TUnion ts           -> Set.unions $ map extractGenericsFromType ts
        TIntersection ts    -> Set.unions $ map extractGenericsFromType ts
        _                   -> Set.empty
      Nothing -> Set.empty  -- TyVar case

-- | Check if a type is the given variable
isVariable :: Variable -> UnsolvedTy -> Bool
isVariable var ty = case var of
  GenericVar name -> isGenericNamed name ty
  TypeVar tyVar -> case projectFix ty :: Maybe (TyVarF UnsolvedTy) of
    Just (TyVar v) -> v == tyVar
    Nothing        -> False

-- | Calculate bounds for any variable (generic or type var)
calculateBoundsForVar :: Variable -> Set Assumption -> Bounds
calculateBoundsForVar var assumptions =
  let lowerBounds = mapMaybe (getLowerBoundForVar var) $ Set.toList assumptions
      upperBounds = mapMaybe (getUpperBoundForVar var) $ Set.toList assumptions
   in Bounds lowerBounds upperBounds

-- | Get lower bound for a variable from a constraint
getLowerBoundForVar :: Variable -> Constraint -> Maybe UnsolvedTy
getLowerBoundForVar var = \case
  IsSubtypeOf t1 t2 | isVariable var t2 -> Just t1
  _ -> Nothing

-- | Get upper bound for a variable from a constraint
getUpperBoundForVar :: Variable -> Constraint -> Maybe UnsolvedTy
getUpperBoundForVar var = \case
  IsSubtypeOf t1 t2 | isVariable var t1 -> Just t2
  _ -> Nothing

-- | Single propagation step for assumptions (similar to calculatePropagation)
calculatePropagationAssumptions :: Set Assumption -> Maybe (Set Assumption)
calculatePropagationAssumptions assumptions =
  let vars = extractAllVars assumptions
      boundsMap = Map.fromList [(var, calculateBoundsForVar var assumptions) | var <- Set.toList vars]
      -- Generate new constraints by pairing lower and upper bounds
      newConstraints = concat $ Map.elems $ Map.map boundsToConstraints boundsMap
      -- Filter out existing and trivial constraints
      actuallyNewConstraints = filter (\c -> not (Set.member c assumptions) && not (isTrivialConstraint c)) newConstraints
   in if null actuallyNewConstraints
        then Nothing
        else Just (Set.fromList actuallyNewConstraints)

-- | Recursive propagation (similar to calculatePropagationAll)
calculatePropagationAssumptionsAll :: Set Assumption -> Set Assumption
calculatePropagationAssumptionsAll assumptions =
  case calculatePropagationAssumptions assumptions of
    Nothing    -> -- trace (T.unpack $ "\nFINAL PROPAGATED ASSUMPTIONS\n" <> formatConstraints assumptions) $
      assumptions
    Just newAs -> calculatePropagationAssumptionsAll (Set.union assumptions newAs)
