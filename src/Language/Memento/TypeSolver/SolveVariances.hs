{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.SolveVariances (solveVariances, solveVariancesFromEnv) where


import qualified Data.Map.Strict                            as Map
import qualified Data.Set                                   as Set
import qualified Data.Text                                  as T
import           Language.Memento.Data.Environment.Ty       (TyCons,
                                                             TyConsConstructor (..))
import           Language.Memento.Data.Environment.Variance (Variance (..),
                                                             combineVariance,
                                                             composeVariance)
import           Language.Memento.Data.Functor.Coproduct    (absurdVoidF, (?:))
import           Language.Memento.Data.Functor.FixedPoint   (foldFix)
import           Language.Memento.Data.Ty                   (Ty, TyF (..))


{-
This module solves variances of type constructor.
-}

-- | Variance expression representing how a type parameter appears
data VarianceExpr
  = VarE T.Text                -- ^ Variable reference (recursive type)
  | ConstE Variance            -- ^ Constant variance value
  | CombineE [VarianceExpr]    -- ^ Addition (+) - parameter appears multiple times
  | ComposeE [VarianceExpr]    -- ^ Multiplication (*) - parameter appears nested
  deriving (Eq, Show)

{-
REFERENCE

-- Analyze variance of a specific type parameter in a type expression
analyzeParameterVariance :: TypeConstructorVariances -> T.Text -> Type -> Variance
analyzeParameterVariance varsMap paramName typeExpr = case typeExpr of
  -- Base cases - no variance contribution
  TNumber -> Bivariant
  TBool -> Bivariant
  TString -> Bivariant
  TNever -> Bivariant
  TUnknown -> Bivariant
  TLiteral _ -> Bivariant
  TVar _ -> Bivariant
  -- Generic type parameter - this is where we find our target!
  TGeneric name
    | name == paramName -> Covariant
    | otherwise -> Invariant
  -- Function type - argument is contravariant, return is covariant
  TFunction argsType retType ->
    let argVariances = map (flipVariance . analyzeParameterVariance varsMap paramName) argsType
        retVariance = analyzeParameterVariance varsMap paramName retType
     in foldl combineVariance retVariance argVariances
  -- Union type - maintains current variance for all members
  TUnion types ->
    let variances = map (analyzeParameterVariance varsMap paramName) (Set.toList types)
     in foldl combineVariance Bivariant variances
  -- Intersection type - maintains current variance for all members
  TIntersection types ->
    let variances = map (analyzeParameterVariance varsMap paramName) (Set.toList types)
     in foldl combineVariance Bivariant variances
  -- Type application - need to look up variance of the other types
  TApplication name argTypes
    | Just variances <- Map.lookup name varsMap ->
        let argVariances =
              zipWith
                ( \arg var ->
                    composeVariance var $
                      analyzeParameterVariance
                        varsMap
                        paramName
                        arg
                )
                argTypes
                variances
         in foldl combineVariance Bivariant argVariances
    | otherwise -> Invariant -- No variance info for this constructor

-}

-- | Solve TyCons variances!
-- | Maybe Variances ->
-- |  if Just v, then use v
-- |  if Nothing, then annotate with renew VarE
-- | Generate equations (Map.Map T.Text VarianceExpr) for each value constructor in type constructor.
-- | NOTE: for now, we generate equations for singleton generics. (otherwise, invariant) For example)
-- | data Typ<auto, auto, auto> { fn mk<T> (T, fn (T) -> T) -> Typ<T, number, T | bool> }
-- |    where Typ's variances in environment is: <$v1, $v2, $v3> (replaced from Nothing)
-- | then we get these equations:
-- |  1. $v1 |-> ConstE Covariant -- 1st arg, 1st generic
-- |  2. $v1 |-> CombinE[ ComposeE [ConstE Contravariant, ConstE Covariant], ComposeE [ConstE Covariant, ConstE Covariant] ] -- 2st arg, 1nd generic (in 2nd arg of mk, T is in "contravariant" position, which is recursively computed via ComposeE)
-- |  2. $v2 |-> ConstE Invariant -- 2nd generic   -- We ignore "number", because it is not a generic type parameter.
-- |  3. $v3 |-> ConstE Invariant -- 3rd generic   -- We ignore "bool", because it is not a generic type parameter.
-- | And solve.
solveVariancesFromEnv :: TyCons (Maybe Variance) -> TyCons Variance
solveVariancesFromEnv tyCons = solvedTyCons
  where
    -- Generate unique variable names for each Nothing variance
    varNames :: Map.Map (T.Text, Int) T.Text
    varNames = Map.fromList
      [ ((tyConName, idx), "$" <> tyConName <> "_" <> T.pack (show idx))
      | (tyConName, (variances, _)) <- Map.toList tyCons
      , (idx, Nothing) <- zip [0..] variances
      ]

    -- Replace Maybe Variance with either the given variance or a variable expression
    replaceWithVars :: T.Text -> [Maybe Variance] -> [VarianceExpr]
    replaceWithVars tyConName variances =
      [ case maybeVar of
          Just v  -> ConstE v
          Nothing -> VarE $ varNames Map.! (tyConName, idx)
      | (idx, maybeVar) <- zip [0..] variances
      ]

    -- Generate equations for all type constructors
    allEquations :: Map.Map T.Text VarianceExpr
    allEquations = Map.unions
      [ generateEquationsForTyCon tyConName vars constructors
      | (tyConName, (maybeVars, constructors)) <- Map.toList tyCons
      , let vars = replaceWithVars tyConName maybeVars
      ]

    -- Generate equations for a single type constructor
    generateEquationsForTyCon :: T.Text -> [VarianceExpr] -> [TyConsConstructor] -> Map.Map T.Text VarianceExpr
    generateEquationsForTyCon tyConName vars constructors =
      Map.unionsWith (\e1 e2 -> CombineE [e1, e2])
      [ generateEquationsForConstructor tyConName vars constructor
      | constructor <- constructors
      ]

    -- Generate equations for a single value constructor
    generateEquationsForConstructor :: T.Text -> [VarianceExpr] -> TyConsConstructor -> Map.Map T.Text VarianceExpr
    generateEquationsForConstructor _tyConName vars constructor =
      let
        -- Get the generics for this value constructor
        valConGenerics = Set.fromList (tccGenerics constructor)

        -- Analyze arguments (contravariant position) 
        argEquations = Map.unionsWith (\e1 e2 -> CombineE [e1, e2])
          [ analyzeTypeForGenerics valConGenerics Contravariant ty
          | ty <- tccArgs constructor
          ]

        -- Analyze return type arguments and map to type constructor parameters
        retEquations = Map.unionsWith (\e1 e2 -> CombineE [e1, e2])
          [ analyzeReturnTypeForParam vars idx ty valConGenerics
          | (idx, ty) <- zip [0..] (tccReturn constructor)
          , idx < length vars
          ]
      in
        Map.unionWith (\e1 e2 -> CombineE [e1, e2]) argEquations retEquations

    -- Analyze how a type parameter appears in return type position
    analyzeReturnTypeForParam :: [VarianceExpr] -> Int -> Ty -> Set.Set T.Text -> Map.Map T.Text VarianceExpr
    analyzeReturnTypeForParam vars paramIdx retTy valConGenerics =
      case vars !! paramIdx of
        VarE varName -> 
          let genericVariances = analyzeTypeForGenerics valConGenerics Covariant retTy
              paramEquationList = Map.elems genericVariances
          in case paramEquationList of
               [] -> Map.empty
               [single] -> Map.singleton varName single
               multiple -> Map.singleton varName (CombineE multiple)
        _ -> Map.empty  -- Already solved variance, no equation needed

    -- Analyze variance of generics in a type at a given position, returning Map of generics to variance expressions
    analyzeTypeForGenerics :: Set.Set T.Text -> Variance -> Ty -> Map.Map T.Text VarianceExpr
    analyzeTypeForGenerics generics currentVariance = foldFix $ (\case
      TGeneric g 
        | Set.member g generics -> Map.singleton g (ConstE currentVariance)
        | otherwise -> Map.empty
      TFunction args ret ->
        let argMaps = map (Map.map (\e -> ComposeE [ConstE Contravariant, e])) args
            retMap = Map.map (\e -> ComposeE [ConstE Covariant, e]) ret
        in Map.unionsWith (\e1 e2 -> CombineE [e1, e2]) (retMap : argMaps)
      TApplication _ args -> 
        Map.unionsWith (\e1 e2 -> CombineE [e1, e2]) args
      TUnion ts -> 
        Map.unionsWith (\e1 e2 -> CombineE [e1, e2]) ts
      TIntersection ts ->
        Map.unionsWith (\e1 e2 -> CombineE [e1, e2]) ts
      _ -> Map.empty
      ) ?: absurdVoidF

    -- Solve the equations
    solvedEquations = solveVariances allEquations

    -- Build the final TyCons with solved variances
    solvedTyCons = Map.mapWithKey (\tyConName (maybeVars, constructors) ->
      let vars = replaceWithVars tyConName maybeVars
          solvedVars = [ case var of
                          VarE varName -> Map.findWithDefault Invariant varName solvedEquations
                          ConstE v -> v
                          _ -> Invariant  -- Fallback for complex expressions
                       | var <- vars
                       ]
      in (solvedVars, constructors)
      ) tyCons

-- | Solve variance equations using fixed-point iteration
solveVariances :: Map.Map T.Text VarianceExpr -> Map.Map T.Text Variance
solveVariances equations = findFixedPoint initial
  where
    -- Start with all variables as Bivariant
    initial = Map.map (const Bivariant) equations

    -- One iteration: evaluate all equations with current assignments
    step :: Map.Map T.Text Variance -> Map.Map T.Text Variance
    step current = Map.map (evalExpr current) equations

    -- Find fixed point
    findFixedPoint :: Map.Map T.Text Variance -> Map.Map T.Text Variance
    findFixedPoint current =
      let next = step current
      in if current == next
         then current
         else findFixedPoint next

    -- Evaluate expression with current variable assignments
    evalExpr :: Map.Map T.Text Variance -> VarianceExpr -> Variance
    evalExpr env expr = case expr of
      VarE x -> Map.findWithDefault Bivariant x env
      ConstE v -> v
      CombineE [] -> Bivariant  -- Identity for combine
      CombineE [e] -> evalExpr env e
      CombineE (e:es) -> combineVariance (evalExpr env e) (evalExpr env (CombineE es))
      ComposeE [] -> Covariant  -- Identity for compose
      ComposeE [e] -> evalExpr env e
      ComposeE (e:es) -> composeVariance (evalExpr env e) (evalExpr env (ComposeE es))
