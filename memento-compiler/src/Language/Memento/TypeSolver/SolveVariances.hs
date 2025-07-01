{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.SolveVariances (VarianceError(..), solveVariances, solveVariancesFromEnv, checkVarianceConsistency) where


import qualified Data.Map.Strict                            as Map
import qualified Data.Text                                  as T
import           Language.Memento.Data.Environment.Ty       (TyCons,
                                                             TyConsConstructor (..))
import           Language.Memento.Data.Environment.Variance (Variance (..),
                                                             combineVariance,
                                                             composeVariance)
import           Language.Memento.Data.Functor.FixedPoint   (projectFix)
import           Language.Memento.Data.Ty                   (Ty, TyF (..))


data VarianceError = VarianceError
  { veTyConsName       :: T.Text
  , veValConsName      :: T.Text
  , veVarianceIdx      :: Int
  , veExpectedVariance :: Variance
  , veActualVariance   :: Variance
  } deriving (Show, Eq, Ord)

{-
This module solves variances of type constructor.
-}

-- | Variance expression representing how a type parameter appears
data VarianceExpr
  = VarE T.Text                -- ^ Variable reference (recursive type)
  | ConstE Variance            -- ^ Constant variance value
  | CombineE [VarianceExpr]    -- ^ Addition (+) - parameter appears multiple times
  | ComposeE [VarianceExpr]    -- ^ Multiplication (*) - parameter appears nested
  deriving (Eq, Show, Ord)

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

    replacedTyCons :: Map.Map T.Text [VarianceExpr] -- Map of type constructor name to its variances
    replacedTyCons = Map.fromList $ map (\(tyConName, (variances, _)) ->
      (tyConName, replaceWithVars tyConName variances)
      ) (Map.toList tyCons)

    -- Generate equations for all type constructors
    allEquations :: Map.Map T.Text VarianceExpr
    allEquations = Map.unions
      [ generateEquationsForTyCon tyConName vars constructors
      | (tyConName, vars) <- Map.toList replacedTyCons,
      let constructors = snd (Map.findWithDefault ([], []) tyConName tyCons)
      ]

    -- Generate equations for a single type constructor
    generateEquationsForTyCon :: T.Text -> [VarianceExpr] -> [TyConsConstructor] -> Map.Map T.Text VarianceExpr
    generateEquationsForTyCon tyConName vars constructors =
      Map.unionsWith (\e1 e2 -> CombineE [e1, e2])
      [ generateEquationsForConstructor tyConName vars constructor
      | constructor <- constructors
      ]

    -- Generate equations for a single value constructor
    -- Vars : [VarianceExpr] - for Ty<auto, in, out>, we get [VarE $tyConName_0, ConstE Contravariant, ConstE Covariant]
    generateEquationsForConstructor :: T.Text -> [VarianceExpr] -> TyConsConstructor -> Map.Map T.Text VarianceExpr
    generateEquationsForConstructor _tyConName vars constructor =
      let
        -- for mk_foo(...) -> Ty<number, T, T | bool>, we get
        -- tyConsGenerics = ["number", "T", "T | bool"]  -- List of Ty
        tyConsGenerics = tccReturn constructor

        -- for mk_foo(string, T, T | bool) -> Ty<...>, we get
        -- tccArgs = [string, T, T | bool]  -- List of
        valueConsArgs = tccArgs constructor
      in
        Map.fromList $ [(varName, expr) |
          (VarE varName, tyConsGeneric) <- zip vars tyConsGenerics,
          let expr = case projectFix tyConsGeneric of
                        Just (TGeneric g) -> CombineE $ map (analyzeVarianceExpr replacedTyCons g) valueConsArgs
                        _                 -> ConstE Invariant  -- Non-generic types are invariant
          ]

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


analyzeVarianceExpr :: Map.Map T.Text [VarianceExpr] -> T.Text -> Ty -> VarianceExpr
analyzeVarianceExpr replacedTyCons genericName ty = case projectFix ty of
      Just (TGeneric g)
        | g == genericName -> ConstE Covariant  -- Generic type parameter is covariant
        | otherwise -> ConstE Bivariant -- Non-generic type parameter is bivariant (no-usage)
      Just (TFunction args ret) ->
        let argExprs = map (analyzeVarianceExpr replacedTyCons genericName) args
            retExpr = analyzeVarianceExpr replacedTyCons genericName ret
        in CombineE (retExpr : map (ComposeE . (: [ConstE Contravariant])) argExprs)  -- Function arguments are contravariant, return is covariant
      Just (TApplication tyConsName args) ->
        let
          tyConsVariances = Map.lookup tyConsName replacedTyCons
        in case tyConsVariances of
          Just varExprs ->
            CombineE
              $ zipWith (\ varExpr arg -> ComposeE (varExpr : [analyzeVarianceExpr replacedTyCons genericName arg])) varExprs args
          Nothing -> ConstE Invariant  -- No variance info for this type constructor
      Just (TUnion ts) ->
        CombineE $ map (analyzeVarianceExpr replacedTyCons genericName) ts  -- Union types are bivariant
      Just (TIntersection ts) ->
        CombineE $ map (analyzeVarianceExpr replacedTyCons genericName) ts  -- Intersection types are bivariant
      Just (TLiteral _) -> ConstE Bivariant  -- Literal types are bivariant
      Just TNumber -> ConstE Bivariant  -- Number type is bivari
      Just TInt -> ConstE Bivariant  -- Number type is bivari
      Just TBool -> ConstE Bivariant  -- Bool type is bivariant
      Just TString -> ConstE Bivariant  -- String type is bivari
      Just TNever -> ConstE Bivariant  -- Never type is bivari
      Just TUnknown -> ConstE Bivariant  -- Unknown type is bivari
      Nothing -> ConstE Invariant  -- Fallback for unsupported types

-- | solveVariancesFromEnv のメイン処理はユーザーから指定された Variance に関する不等式に関してなんら関与しないので、
-- | 最終的に作られた Variance List が矛盾を起こすことがある。
-- | その時のための検知用
-- | 返り値はエラーリスト of (型コンストラクタ名, 値コンストラクタ名, バリアンスのインデックス) (空なら成功)
checkVarianceConsistency :: TyCons Variance -> [VarianceError]
checkVarianceConsistency tyCons =
  let
      -- Compute Text to VarianceExpr map fromTyCons
      solvedVariances :: Map.Map T.Text [VarianceExpr]
      solvedVariances = Map.map (map ConstE . fst) tyCons
  in [ VarianceError
        { veTyConsName  = name
        , veValConsName = constructorName
        , veVarianceIdx = idx
        , veExpectedVariance = expectedVar
        , veActualVariance = var
        }
      | (name, (vars, constructors)) <- Map.toList tyCons,
        constructor <- constructors,
        -- Now we get name, idx, var
        let
          constructorName = tccName constructor
          constructorArgs = tccArgs constructor
          constructorReturnGens = tccReturn constructor,
        (idx, var, constructorReturnGen) <- zip3 [0 ..] vars constructorReturnGens -- Variances
        , let varExpr = case projectFix constructorReturnGen of
                Just (TGeneric g) -> CombineE $ map (analyzeVarianceExpr solvedVariances g) constructorArgs
                _                 -> ConstE Invariant  -- Non-generic types are invariant
              expectedVar = evalExpr Map.empty varExpr
        , -- ユーザー指定の Variance が、期待される Variance より小さくなければいけない
          -- 例えば Covariance が期待されている場所に Invariance を突っ込むことは可能
          not $ var `isSubVarianceOf` expectedVar
      ]

-- | Variance Lattice
isSubVarianceOf :: Variance -> Variance -> Bool
isSubVarianceOf v1 v2 = case (v1, v2) of
  (Covariant, Covariant)         -> True
  (Contravariant, Contravariant) -> True
  (Invariant, Invariant)         -> True
  (Bivariant, Bivariant)         -> True
  (Covariant, Bivariant)         -> True
  (Contravariant, Bivariant)     -> True
  (Invariant, Bivariant)         -> True
  (Invariant, Covariant)         -> True
  (Invariant, Contravariant)     -> True
  _                              -> False

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
