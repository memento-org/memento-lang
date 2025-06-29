{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Language.Memento.TypeSolver.ConstraintGen (generateConstraints) where

{-
This module defines the function `TypedAST UnsolvedTy -> Set Constraint`.
-}

import           Control.Monad                                   (unless)
import           Control.Monad.State                             (State,
                                                                  execState,
                                                                  modify)
import           Data.Foldable                                   (forM_)
import           Data.Maybe                                      (mapMaybe)
import           Data.Set                                        (Set)
import qualified Data.Set                                        as Set
import           GHC.Base                                        (List)
import           Language.Memento.Data.AST                       (Syntax)
import           Language.Memento.Data.AST.Definition            (Definition (..))
import           Language.Memento.Data.AST.Expr                  (BinOp (..),
                                                                  Block (Block),
                                                                  Expr (..),
                                                                  Let (Let))
import           Language.Memento.Data.AST.Literal               (Literal (..))
import           Language.Memento.Data.AST.Pattern               (Pattern (..))
import           Language.Memento.Data.AST.Program               (Program (Program))
import           Language.Memento.Data.AST.Tag                   (KBlock, KExpr,
                                                                  KPattern,
                                                                  KProgram)
import           Language.Memento.Data.Functor.FixedPoint        (injectFix)
import           Language.Memento.Data.Functor.FixedPoint.Higher (safeProjectVia)
import           Language.Memento.Data.Ty                        (TyF (..),
                                                                  UnsolvedTy)
import           Language.Memento.Data.TypedAST                  (TyInfo,
                                                                  TypedAST)
import           Language.Memento.Data.TypedAST.TyInfo           (BlockTyInfo (BlockTyInfo),
                                                                  ExprTyInfo (..),
                                                                  PatternTyInfo (PatternTyInfo),
                                                                  VariableTyInfo (VariableTyInfo))
import           Language.Memento.TypeSolver.Data.Constraint     (Assumption,
                                                                  Constraint (..),
                                                                  (?<:))

data ConstraintGenState = ConstraintGenState {
  cgsConstraints   :: Set Constraint
  , cgsAssumptions :: Set Assumption
  }

type ConstraintGenM a = State ConstraintGenState a

initState :: ConstraintGenState
initState = ConstraintGenState {
  cgsConstraints   = Set.empty
  , cgsAssumptions = Set.empty
  }

addConstraint :: Constraint -> ConstraintGenM ()
addConstraint c@(l `IsSubtypeOf` r) = do
  unless (l == r) $ modify $ \s -> s { cgsConstraints = Set.insert c (cgsConstraints s) }

addAssumption :: Assumption -> ConstraintGenM ()
addAssumption c@(l `IsSubtypeOf` r) = do
  unless (l == r) $ modify $ \s -> s { cgsAssumptions = Set.insert c (cgsAssumptions s) }

runConstraintGenM :: ConstraintGenM a -> (Set Assumption, Set Constraint)
runConstraintGenM m = let
  initialState = initState
  finalState = execState m initialState
  in (cgsAssumptions finalState, cgsConstraints finalState)

-- | Generate list of pair of (set of assumptions and sets of constraints) (for each value or function definition)
generateConstraints :: TypedAST UnsolvedTy KProgram -> List (Set Assumption, Set Constraint)
generateConstraints ast = case safeProjectVia @Syntax ast of
  Program definitions -> mapMaybe (
    \def -> case safeProjectVia @Syntax def of
      ValDef varAST _ _ exprAST ->
        let
          -- | val x<T> : F<T> = expr; -- variable type info uses its generic type F<T>
          VariableTyInfo varTy = safeProjectVia @(TyInfo UnsolvedTy) varAST
          ExprTyInfo exprTy = safeProjectVia @(TyInfo UnsolvedTy) exprAST
        in Just $ runConstraintGenM $ do
            addConstraint (exprTy ?<: varTy)
            generateExprConstraints exprAST
      FnDef varAST _ params _ blockAST ->
        let
          -- | fn x<T>(params : F<T>) -> G<T> = expr; -- function type info uses its generic type F<T> -> G<T>
          VariableTyInfo varTy = safeProjectVia @(TyInfo UnsolvedTy) varAST
          BlockTyInfo blockTy = safeProjectVia @(TyInfo UnsolvedTy) blockAST
          paramsTy = map ((\(VariableTyInfo argTy) -> argTy) . (\(argVarAST, _) -> safeProjectVia @(TyInfo UnsolvedTy) argVarAST)) params
          functionTy = injectFix $ TFunction paramsTy blockTy
        in Just $ runConstraintGenM $ do
          addConstraint (functionTy ?<: varTy)
          generateBlockConstraints blockAST
      _ -> Nothing
    ) definitions

generateExprConstraints :: TypedAST UnsolvedTy KExpr -> ConstraintGenM ()
generateExprConstraints ast =
  let
    ExprTyInfo exprTy = safeProjectVia @(TyInfo UnsolvedTy) ast
  in
    case safeProjectVia @Syntax ast of
      EVar varAST ->
        let
          VariableTyInfo varTy = safeProjectVia @(TyInfo UnsolvedTy) varAST
        in addConstraint $ varTy ?<: exprTy
      EBinOp op leftAST rightAST ->
        let
          ExprTyInfo leftTy = safeProjectVia @(TyInfo UnsolvedTy) leftAST
          ExprTyInfo rightTy = safeProjectVia @(TyInfo UnsolvedTy) rightAST
          (leftExpected, rightExpected, retExpected) = (\(l,r,ret) -> (injectFix l, injectFix r, injectFix ret)) $ case op of
            Add -> (TNumber, TNumber, TNumber)
            Sub -> (TNumber, TNumber, TNumber)
            Mul -> (TNumber, TNumber, TNumber)
            Div -> (TNumber, TNumber, TNumber)
            Eq  -> (TUnknown, TUnknown, TBool)
            Lt  -> (TNumber, TNumber, TBool)
            Gt  -> (TNumber, TNumber, TBool)
        in do
          addConstraint $ leftTy ?<: leftExpected
          addConstraint $ rightTy ?<: rightExpected
          addConstraint $ retExpected ?<: exprTy
      ELiteral literalAST -> case safeProjectVia @Syntax literalAST of
        NumberLiteral _ -> addConstraint $ injectFix TNumber ?<: exprTy
        IntLiteral _    -> addConstraint $ injectFix TInt ?<: exprTy
        BoolLiteral _   -> addConstraint $ injectFix TBool ?<: exprTy
        StringLiteral _ -> addConstraint $ injectFix TString ?<: exprTy
      ELambda params bodyAST ->
        let
          ExprTyInfo bodyTy = safeProjectVia @(TyInfo UnsolvedTy) bodyAST
          paramsTy = map (\(paramAST, _) ->
            let PatternTyInfo paramTy = safeProjectVia @(TyInfo UnsolvedTy) paramAST
            in paramTy) params
          lambdaTy = injectFix $ TFunction paramsTy bodyTy
        in do
          addConstraint $ lambdaTy ?<: exprTy
          generateExprConstraints bodyAST
          mapM_ (generatePatternAssumptions . fst) params
      EApply funcAST argsAST ->
        let
          ExprTyInfo funcTy = safeProjectVia @(TyInfo UnsolvedTy) funcAST
          argsTy = map (\argAST ->
            let ExprTyInfo argTy = safeProjectVia @(TyInfo UnsolvedTy) argAST
            in argTy) argsAST
          functionTy = injectFix $ TFunction argsTy exprTy
        in do
          mapM_ generateExprConstraints argsAST
          addConstraint $ funcTy ?<: functionTy
          generateExprConstraints funcAST
      EIf condAST thenAST elseAST ->
        let
          ExprTyInfo condTy = safeProjectVia @(TyInfo UnsolvedTy) condAST
          ExprTyInfo thenTy = safeProjectVia @(TyInfo UnsolvedTy) thenAST
          ExprTyInfo elseTy = safeProjectVia @(TyInfo UnsolvedTy) elseAST
          boolTy = injectFix TBool
        in do
          addConstraint $ condTy ?<: boolTy
          addConstraint $ thenTy ?<: exprTy
          addConstraint $ elseTy ?<: exprTy
          generateExprConstraints condAST
          generateExprConstraints thenAST
          generateExprConstraints elseAST
      EMatch scrutinees clauses ->
        let
          scrutineesTy = map (\exprAST ->
            let ExprTyInfo scrutineeTy = safeProjectVia @(TyInfo UnsolvedTy) exprAST
            in scrutineeTy) scrutinees
        in forM_ clauses $ \(patternAndTypes, caseExpr) -> do
          let
            ExprTyInfo caseExprTy = safeProjectVia @(TyInfo UnsolvedTy) caseExpr
            patternsTy = map (\(patternAST, _) ->
              let PatternTyInfo patTy = safeProjectVia @(TyInfo UnsolvedTy) patternAST
              in patTy) patternAndTypes
          addConstraint $ caseExprTy ?<: exprTy
          forM_ (zip scrutineesTy patternsTy) $ \(scrutineeTy, patTy) ->
            addConstraint $ scrutineeTy ?<: patTy
          mapM_ (generatePatternAssumptions . fst) patternAndTypes
      EBlock blockAST ->
        let
          BlockTyInfo blockTy = safeProjectVia @(TyInfo UnsolvedTy) blockAST
        in do
          addConstraint $ blockTy ?<: exprTy
          generateBlockConstraints blockAST

generateBlockConstraints :: TypedAST UnsolvedTy KBlock -> ConstraintGenM ()
generateBlockConstraints ast =
  let
    BlockTyInfo blockTy = safeProjectVia @(TyInfo UnsolvedTy) ast
  in case safeProjectVia @Syntax ast of
    Block lets lastExpr -> do
      forM_ lets $ \letAST -> case safeProjectVia @Syntax letAST of
        Let patAST _ exprAST -> do
          let PatternTyInfo patTy = safeProjectVia @(TyInfo UnsolvedTy) patAST
          let ExprTyInfo exprTy = safeProjectVia @(TyInfo UnsolvedTy) exprAST
          addConstraint $ exprTy ?<: patTy -- Type of expression must be subtype of pattern type
          generateExprConstraints exprAST
          generatePatternAssumptions patAST -- Generate assumptions for the pattern
      generateExprConstraints lastExpr -- Generate constraints for the last expression
      let ExprTyInfo lastExprTy = safeProjectVia @(TyInfo UnsolvedTy) lastExpr
      addConstraint $ lastExprTy ?<: blockTy -- Type of the last expression must be subtype of block type

generatePatternAssumptions :: TypedAST UnsolvedTy KPattern -> ConstraintGenM ()
generatePatternAssumptions ast =
  let
    PatternTyInfo patTy = safeProjectVia @(TyInfo UnsolvedTy) ast
  in case safeProjectVia @Syntax ast of
    PVar varAST -> do
      let VariableTyInfo varTy = safeProjectVia @(TyInfo UnsolvedTy) varAST
      addAssumption $ varTy ?<: patTy -- Type solver can known that the variable has more specific type than the pattern type
    PLiteral literalAST -> case safeProjectVia @Syntax literalAST of
      NumberLiteral _ -> addAssumption $ injectFix TNumber ?<: patTy
      IntLiteral _    -> addAssumption $ injectFix TInt ?<: patTy
      BoolLiteral _   -> addAssumption $ injectFix TBool ?<: patTy
      StringLiteral _ -> addAssumption $ injectFix TString ?<: patTy
    PWildcard -> pure () -- Wildcard patterns do not generate assumptions
    PCons consVarAST args -> do
      let
        VariableTyInfo consVarTy = safeProjectVia @(TyInfo UnsolvedTy) consVarAST
        argsTy = map (\argAST ->
          let PatternTyInfo argTy = safeProjectVia @(TyInfo UnsolvedTy) argAST
          in argTy) args
      addAssumption $ consVarTy ?<: injectFix (TFunction argsTy patTy) -- Cons pattern generates assumption that the constructor function has type TFunction argsTy patTy
      mapM_ generatePatternAssumptions args -- Generate assumptions for each argument pattern
