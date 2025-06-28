{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Language.Memento.Backend.JS.Compile (
  compileToJS,
  compileExpr,
  compileLiteral,
  compileStmt,
) where

import           Control.Monad                                   (zipWithM)
import           Control.Monad.State                             (State, get,
                                                                  modify,
                                                                  runState)
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Text                                       (Text)
import           Language.Memento.Backend.JS.Data.IR
import           Language.Memento.Data.AST                       (Syntax)
import           Language.Memento.Data.AST.Definition            (ConstructorDef (..),
                                                                  Definition (..))
import           Language.Memento.Data.AST.Expr                  (BinOp (..),
                                                                  Block (..),
                                                                  Expr (..),
                                                                  Let (..))
import           Language.Memento.Data.AST.Literal               (Literal (..))
import           Language.Memento.Data.AST.Pattern               (Pattern (..))
import           Language.Memento.Data.AST.Program               (Program (..))
import           Language.Memento.Data.AST.Tag
import           Language.Memento.Data.AST.Variable              (Variable (..))
import           Language.Memento.Data.Functor.Coproduct.Higher  (HInhabitOnly (..),
                                                                  SafeHProjective (..))
import           Language.Memento.Data.Functor.FixedPoint.Higher (extractHFix)
import           Language.Memento.Data.TypedAST                  (TypedAST)

-- | Compilation state for generating unique variable names
data CompileState = CompileState
  { symbolCounter      :: Int
  , constructorSymbols :: Map Text JSSymbol  -- Map from constructor name to its symbol
  }

type CompileM = State CompileState

-- | Pattern matching predicate - a condition to test
data PatternPredicate
  = SymbolCheck JSExpr JSSymbol        -- value[0] === Symbol("name")
  | LiteralCheck JSExpr JSLiteral      -- value === literal
  | AlwaysTrue                         -- wildcard pattern
  deriving (Show, Eq)

-- | Pattern binding - a variable assignment
data PatternBinding = PatternBinding
  { bindingVar  :: JSIdentifier         -- variable name
  , bindingExpr :: JSExpr              -- expression to bind (e.g., value[1])
  } deriving (Show, Eq)

-- | Compiled pattern match case
data CompiledCase = CompiledCase
  { casePredicates :: [PatternPredicate]  -- conditions to check
  , caseBindings   :: [PatternBinding]      -- variables to bind
  , caseBody       :: JSExpr                    -- expression to evaluate
  } deriving (Show, Eq)

initialState :: CompileState
initialState = CompileState 0 Map.empty

-- | Get or create a consistent constructor symbol with $ prefix
getConstructorSymbol :: Text -> CompileM JSSymbol
getConstructorSymbol constructorName = do
  state <- get
  case Map.lookup constructorName (constructorSymbols state) of
    Just symbol -> return symbol
    Nothing -> do
      let prefixedName = "$" <> constructorName
      counter <- symbolCounter <$> get
      let symbol = JSSymbol prefixedName counter
      modify $ \s -> s {
        symbolCounter = counter + 1,
        constructorSymbols = Map.insert constructorName symbol (constructorSymbols s)
      }
      return symbol

-- | Main compilation function: TypedAST -> JavaScript program
compileToJS :: TypedAST t KProgram -> JSProgram
compileToJS typedAST =
  let (jsStmts, finalState) = runState (compileProgram typedAST) initialState
      symbolDefs = generateSymbolDefinitions (constructorSymbols finalState)
      mainCall = if hasMainFunction jsStmts
                 then [JSExprStmt (JSCall (JSMember (JSVar "console") "log") [JSCall (JSVar "main") []])]
                 else []
  in JSProgram (symbolDefs ++ jsStmts ++ mainCall)

-- | Check if the program has a main function
hasMainFunction :: [JSStmt] -> Bool
hasMainFunction = any isMainFunction
  where
    isMainFunction :: JSStmt -> Bool
    isMainFunction (JSConst "main" _) = True
    isMainFunction _                  = False

-- | Generate symbol definitions for constructors
generateSymbolDefinitions :: Map Text JSSymbol -> [JSStmt]
generateSymbolDefinitions symbols =
  map generateSymbolDef (Map.toList symbols)
  where
    generateSymbolDef :: (Text, JSSymbol) -> JSStmt
    generateSymbolDef (_, JSSymbol name _) =
      JSConst name (JSCall (JSVar "Symbol") [JSLit (JSString name)])

-- | Compile a program (top-level)
compileProgram :: TypedAST t KProgram -> CompileM [JSStmt]
compileProgram typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let program = hSafeProject @Program syntax
  case hInhabitOnly @Program @KProgram program of
    Program defs -> concat <$> mapM compileDefinition defs

-- | Compile a definition (function or value)
compileDefinition :: TypedAST t KDefinition -> CompileM [JSStmt]
compileDefinition typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let definition = hSafeProject @Definition syntax
  case hInhabitOnly @Definition @KDefinition definition of
    ValDef name _typeVars _type expr -> do
      jsExpr <- compileExpr expr
      varName <- extractVariableName name
      return [JSConst varName jsExpr]
    FnDef name _typeVars params _retType body -> do
      paramNames <- mapM (extractVariableName . fst) params
      bodyStmts <- compileBlock body
      varName <- extractVariableName name
      let jsFunc = JSFunction paramNames bodyStmts
      return [JSConst varName jsFunc]
    DataDef _name _variances constructors -> do
      -- Generate constructor functions
      mapM compileConstructor constructors
    TypeDef _name _typeVars _type -> do
      -- Type definitions don't generate runtime code
      return []

-- | Compile a constructor to a JavaScript function
compileConstructor :: ConstructorDef (TypedAST t) -> CompileM JSStmt
compileConstructor (ConstructorDef nameAST _typeVars args _retType) = do
  consName <- extractVariableName nameAST
  argNames <- mapM (extractVariableName . fst) args
  symbol <- getConstructorSymbol consName
  let consArgs = map JSVar argNames
  let symbolVar = JSVar (symbolName symbol)
  let consExpr = JSArray (symbolVar : consArgs)
  return $ JSConst consName (JSArrowFunction argNames consExpr)

-- | Compile an expression
compileExpr :: TypedAST t KExpr -> CompileM JSExpr
compileExpr typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let expr = hSafeProject @Expr syntax
  case hInhabitOnly @Expr @KExpr expr of
    EVar var -> do
      varName <- extractVariableName var
      return $ JSVar varName
    ELiteral lit -> compileLiteral lit
    ELambda params body -> do
      paramNames <- mapM (extractPatternName . fst) params
      bodyExpr <- compileExpr body
      return $ JSArrowFunction paramNames bodyExpr
    EApply func args -> do
      jsFunc <- compileExpr func
      jsArgs <- mapM compileExpr args
      return $ JSCall jsFunc jsArgs
    EMatch scrutinees cases -> do
      -- Compile pattern matching to JavaScript if-else statements
      compiledCases <- mapM (compileCase scrutinees) cases
      -- Create an IIFE that contains the pattern matching logic
      let matchStmts = [compileMatchCases compiledCases]
      return $ JSCall (JSFunction [] matchStmts) []
    EIf cond then_ else_ -> do
      jsCond <- compileExpr cond
      jsThen <- compileExpr then_
      jsElse <- compileExpr else_
      return $ JSConditional jsCond jsThen jsElse
    EBinOp op left right -> do
      jsLeft <- compileExpr left
      jsRight <- compileExpr right
      let jsOp = compileBinOp op
      return $ JSBinary jsOp jsLeft jsRight
    EBlock block -> do
      stmts <- compileBlock block
      -- Create an IIFE (Immediately Invoked Function Expression) for block scope
      return $ JSCall (JSFunction [] stmts) []

-- | Compile a literal
compileLiteral :: TypedAST t KLiteral -> CompileM JSExpr
compileLiteral typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let literal = hSafeProject @Literal syntax
  return $ case hInhabitOnly @Literal @KLiteral literal of
    NumberLiteral n -> JSLit (JSNumber n)
    BoolLiteral b   -> JSLit (JSBoolean b)
    StringLiteral s -> JSLit (JSString s)
    IntLiteral i    -> JSLit (JSNumber (fromIntegral i))

-- | Compile a let binding
compileLet :: TypedAST t KLet -> CompileM JSStmt
compileLet typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let letBinding = hSafeProject @Let syntax
  case hInhabitOnly @Let @KLet letBinding of
    Let pattern _maybeType expr -> do
      jsExpr <- compileExpr expr
      varName <- extractPatternName pattern
      return $ JSConst varName jsExpr

-- | Compile a block
compileBlock :: TypedAST t KBlock -> CompileM [JSStmt]
compileBlock typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let block = hSafeProject @Block syntax
  case hInhabitOnly @Block @KBlock block of
    Block lets expr -> do
      letStmts <- mapM compileLet lets
      exprStmt <- do
        jsExpr <- compileExpr expr
        return $ JSReturn (Just jsExpr)
      return $ letStmts ++ [exprStmt]

-- | Compile a statement (wrapper for expressions)
compileStmt :: TypedAST t KExpr -> CompileM JSStmt
compileStmt typedExpr = do
  jsExpr <- compileExpr typedExpr
  return $ JSExprStmt jsExpr

-- | Helper: Convert Memento binary operators to JavaScript
compileBinOp :: BinOp -> JSBinaryOp
compileBinOp = \case
  Add -> JSAdd
  Sub -> JSSubtract
  Mul -> JSMultiply
  Div -> JSDivide
  Eq -> JSEqual
  Lt -> JSLessThan
  Gt -> JSGreaterThan

-- | Helper: Extract variable name from TypedAST
extractVariableName :: TypedAST t KVariable -> CompileM JSIdentifier
extractVariableName typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let var = hSafeProject @Variable syntax
  case hInhabitOnly @Variable @KVariable var of
    Var name -> return name

-- | Helper: Extract pattern name (simplified - assumes variable patterns)
extractPatternName :: TypedAST t KPattern -> CompileM JSIdentifier
extractPatternName typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let pattern = hSafeProject @Pattern syntax
  case hInhabitOnly @Pattern @KPattern pattern of
    PVar var   -> extractVariableName var
    PWildcard  -> return "_wildcard"
    PLiteral _ -> return "_literal"
    PCons _ _  -> return "_constructor"

-- | Analyze a pattern and extract predicates and bindings
analyzePattern :: JSExpr -> TypedAST t KPattern -> CompileM ([PatternPredicate], [PatternBinding])
analyzePattern valueExpr typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let pattern = hSafeProject @Pattern syntax
  case hInhabitOnly @Pattern @KPattern pattern of
    PVar var -> do
      -- Variable pattern: no predicate, bind the whole value
      varName <- extractVariableName var
      return ([], [PatternBinding varName valueExpr])

    PWildcard ->
      -- Wildcard: always matches, no bindings
      return ([AlwaysTrue], [])

    PLiteral lit -> do
      -- Literal pattern: check equality, no bindings
      literal <- compileLiteralPattern lit
      return ([LiteralCheck valueExpr literal], [])

    PCons constructor args -> do
      -- Constructor pattern: check symbol + analyze arguments
      consName <- extractVariableName constructor
      symbol <- getConstructorSymbol consName
      let symbolPred = SymbolCheck (JSIndex valueExpr (JSLit (JSNumber 0))) symbol

      -- Recursively analyze constructor arguments
      (argPreds, argBindings) <- unzip <$> zipWithM analyzeArgPattern args [1..]

      return (symbolPred : concat argPreds, concat argBindings)
  where
    analyzeArgPattern :: TypedAST t KPattern -> Int -> CompileM ([PatternPredicate], [PatternBinding])
    analyzeArgPattern argPattern index =
      analyzePattern (JSIndex valueExpr (JSLit (JSNumber (fromIntegral index)))) argPattern

-- | Compile a literal from a pattern context
compileLiteralPattern :: TypedAST t KLiteral -> CompileM JSLiteral
compileLiteralPattern typedAST = do
  let syntax = extractHFix @Syntax typedAST
  let literal = hSafeProject @Literal syntax
  return $ case hInhabitOnly @Literal @KLiteral literal of
    NumberLiteral n -> JSNumber n
    BoolLiteral b   -> JSBoolean b
    StringLiteral s -> JSString s
    IntLiteral i    -> JSNumber (fromIntegral i)

-- | Convert pattern predicate to JavaScript expression
predicateToJS :: PatternPredicate -> JSExpr
predicateToJS = \case
  SymbolCheck expr symbol ->
    JSBinary JSEqual expr (JSVar (symbolName symbol))
  LiteralCheck expr literal ->
    JSBinary JSEqual expr (JSLit literal)
  AlwaysTrue ->
    JSLit (JSBoolean True)

-- | Convert pattern bindings to JavaScript statements
bindingsToJS :: [PatternBinding] -> [JSStmt]
bindingsToJS bindings =
  map (\(PatternBinding var expr) -> JSConst var expr) bindings

-- | Compile a match case to JavaScript
compileMatchCase :: CompiledCase -> JSStmt
compileMatchCase (CompiledCase predicates bindings body) =
  let
    -- Combine all predicates with AND
    condition = case predicates of
      []  -> JSLit (JSBoolean True)
      [p] -> predicateToJS p
      ps  -> foldl1 (JSBinary JSAnd) (map predicateToJS ps)

    -- Create bindings and return statement
    bindingStmts = bindingsToJS bindings
    returnStmt = JSReturn (Just body)
    bodyStmts = bindingStmts ++ [returnStmt]
  in
    JSIf condition bodyStmts Nothing

-- | Compile multiple match cases to nested if-else statements
compileMatchCases :: [CompiledCase] -> JSStmt
compileMatchCases [] = JSThrow (JSLit (JSString "No pattern matched"))
compileMatchCases [c] = compileMatchCase c
compileMatchCases (c:cs) =
  let
    CompiledCase predicates bindings body = c
    condition = case predicates of
      []  -> JSLit (JSBoolean True)
      [p] -> predicateToJS p
      ps  -> foldl1 (JSBinary JSAnd) (map predicateToJS ps)

    bindingStmts = bindingsToJS bindings
    returnStmt = JSReturn (Just body)
    thenStmts = bindingStmts ++ [returnStmt]
    elseStmts = [compileMatchCases cs]
  in
    JSIf condition thenStmts (Just elseStmts)

-- | Compile a single pattern match case
compileCase :: [TypedAST t KExpr] -> ([(TypedAST t KPattern, Maybe (TypedAST t KType))], TypedAST t KExpr) -> CompileM CompiledCase
compileCase scrutinees (patternsWithTypes, caseExpr) = do
  -- Compile scrutinees to JavaScript expressions
  scrutineeExprs <- mapM compileExpr scrutinees

  -- Extract just the patterns (ignore type annotations for now)
  let patterns = map fst patternsWithTypes

  -- Analyze each pattern against its corresponding scrutinee
  analysisResults <- zipWithM analyzePattern scrutineeExprs patterns

  -- Collect all predicates and bindings
  let (allPredicates, allBindings) = unzip analysisResults
  let flatPredicates = concat allPredicates
  let flatBindings = concat allBindings

  -- Compile the case expression
  compiledCaseExpr <- compileExpr caseExpr

  return $ CompiledCase flatPredicates flatBindings compiledCaseExpr
