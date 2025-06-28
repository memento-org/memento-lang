{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Memento.Typing.Inference (
  -- Type inference functions
  typeType,
  typeTypeAST,
  typeTypeVariable,
  getLiteralType,

  -- Expression typing
  typeExpression,
  typeBlock,
  typeLet,

  -- Pattern typing
  typePattern,
  typeLiteral,
  getPatternType,
  findConstructor,

  -- Definition typing
  typeDefinition,
  typeConstructorDef,
  typeVariable,
  introTypeVariable,
  topLevelTypeVariable
) where

import           Control.Monad.Except                            (throwError)
import           Control.Monad.State                             (get, put)
import qualified Data.Map                                        as Map
import qualified Data.Set                                        as Set
import           Data.Text                                       (Text)
import           Language.Memento.Data.AST                       (AST, Syntax)
import           Language.Memento.Data.AST.Definition            (ConstructorDef (ConstructorDef),
                                                                  Definition (DataDef, FnDef, TypeDef, ValDef))
import           Language.Memento.Data.AST.Expr                  (Block (Block),
                                                                  Expr (..),
                                                                  Let (Let))
import           Language.Memento.Data.AST.Literal               (Literal (..))
import           Language.Memento.Data.AST.Metadata              (Metadata)
import           Language.Memento.Data.AST.MType                 (MType (..))
import           Language.Memento.Data.AST.Pattern               (Pattern (..))
import           Language.Memento.Data.AST.Tag                   (KBlock,
                                                                  KDefinition,
                                                                  KExpr, KLet,
                                                                  KLiteral,
                                                                  KPattern,
                                                                  KType,
                                                                  KTypeVariable,
                                                                  KVariable)
import           Language.Memento.Data.AST.Variable              (TypeVariable (TypeVar),
                                                                  Variable (Var))
import           Language.Memento.Data.Environment.Ty            (TyCons,
                                                                  TyConsConstructor (..))
import           Language.Memento.Data.Environment.Variance      (Variance)
import           Language.Memento.Data.Functor.Coproduct.Higher  (HInjective (hInject),
                                                                  SafeHProjective (hSafeProject))
import           Language.Memento.Data.Functor.FixedPoint        (injectFix)
import           Language.Memento.Data.Functor.FixedPoint.Higher (HFix (..),
                                                                  extractHFix,
                                                                  safeProjectVia)
import           Language.Memento.Data.Functor.Higher            (HPhantom (hCoerce))
import           Language.Memento.Data.Functor.Product.Higher    (HUnit (..),
                                                                  (:**:) (..))
import           Language.Memento.Data.Ty                        (Ty,
                                                                  TypeScheme (..),
                                                                  UnsolvedTy,
                                                                  tyToUnsolvedTy)
import qualified Language.Memento.Data.Ty                        as Ty
import           Language.Memento.Data.TypedAST                  (TyInfo,
                                                                  TypedAST)
import           Language.Memento.Data.TypedAST.TyInfo           (BlockTyInfo (BlockTyInfo),
                                                                  DefinitionTyInfo (DefinitionTyInfo),
                                                                  ExprTyInfo (ExprTyInfo),
                                                                  LetTyInfo (LetTyInfo),
                                                                  LiteralTyInfo (LiteralTyInfo),
                                                                  PatternTyInfo (PatternTyInfo),
                                                                  TypeTyInfo (TypeTyInfo),
                                                                  TypeVariableTyInfo (TypeVariableTyInfo),
                                                                  VariableTyInfo (VariableTyInfo))
import           Language.Memento.Typing.Core                    (TypingError (..),
                                                                  TypingM,
                                                                  TypingState (..),
                                                                  extractSourcePos,
                                                                  freshTyVar)
import           Language.Memento.Typing.Scheme                  (instantiateScheme)

-- Type operations
typeType :: AST KType -> TypingM Ty
typeType ast = case safeProjectVia @Syntax ast of
  TVar typeVarAST -> do
    let TypeVar name = safeProjectVia @Syntax typeVarAST
    state <- get
    if Set.member name (tsTypeGenerics state)
      then return $ injectFix $ Ty.TGeneric name
      else case  Map.lookup name (tsTypeConstructors state) of
        Just (variances, _)  ->
          let actualArity = length variances
          in if actualArity == 0 then
            return $ injectFix $ Ty.TApplication name []
          else
            throwError (ArityMismatch name 0 actualArity (extractSourcePos typeVarAST))
        _ -> throwError (TypeVariableNotInScope name (extractSourcePos typeVarAST))
  TNumber -> return $ injectFix Ty.TNumber
  TInt -> return $ injectFix Ty.TInt
  TBool -> return $ injectFix Ty.TBool
  TString -> return $ injectFix Ty.TString
  TFunction argVarTypes retType -> do
    typedArgTypes <- mapM (\(_, argTypeAST) -> typeType argTypeAST) argVarTypes
    typedRetType <- typeType retType
    return $ injectFix $ Ty.TFunction typedArgTypes typedRetType
  TUnknown -> return $ injectFix Ty.TUnknown
  TNever -> return $ injectFix Ty.TNever
  TLiteral literalAST -> do
    let lit = hSafeProject @Literal $ extractHFix @Syntax literalAST
    return $ injectFix $ case lit of
      NumberLiteral _ -> Ty.TNumber
      IntLiteral _    -> Ty.TInt
      BoolLiteral _   -> Ty.TBool
      StringLiteral _ -> Ty.TString
  TUnion types -> do
    typedTypes <- mapM typeType types
    return $ injectFix $ Ty.TUnion typedTypes
  TIntersection types -> do
    typedTypes <- mapM typeType types
    return $ injectFix $ Ty.TIntersection typedTypes
  TApplication nameAST args -> do
    let TypeVar name = hSafeProject @TypeVariable $ extractHFix @Syntax nameAST
    state <- get
    case Map.lookup name (tsTypeConstructors state) of
      Nothing -> throwError (UndefinedTypeConstructor name (extractSourcePos nameAST))
      Just (variances, _) -> do
        let expectedArity = length variances
        let actualArity = length args
        if expectedArity /= actualArity
          then throwError (ArityMismatch name expectedArity actualArity (extractSourcePos nameAST))
          else do
            typedArgs <- mapM typeType args
            return $ injectFix $ Ty.TApplication name typedArgs

typeTypeAST :: AST KType -> TypingM (TypedAST UnsolvedTy KType)
typeTypeAST ast = let meta = extractHFix @Metadata ast in case safeProjectVia @Syntax ast of
  TVar typeVarAST -> do
    typedTypeVar <- typeTypeVariable typeVarAST
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject (TVar typedTypeVar) :**: HUnit
  TNumber -> do
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject TNumber :**: HUnit
  TInt -> do
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject TInt :**: HUnit
  TBool -> do
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject TBool :**: HUnit
  TString -> do
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject TString :**: HUnit
  TFunction params retType -> do
    typedParams <- mapM (\(varAST, typeAST) -> do
      typedVar <- typeFunctionParameter varAST  -- Use helper instead of typeVariable
      typedType <- typeTypeAST typeAST
      return (typedVar, typedType)) params
    typedRetType <- typeTypeAST retType
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject (TFunction typedParams typedRetType) :**: HUnit
  TUnknown -> do
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject TUnknown :**: HUnit
  TNever -> do
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject TNever :**: HUnit
  TLiteral literalAST -> do
    typedLiteral <- typeLiteral literalAST
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject (TLiteral typedLiteral) :**: HUnit
  TUnion types -> do
    typedTypes <- mapM typeTypeAST types
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject (TUnion typedTypes) :**: HUnit
  TIntersection types -> do
    typedTypes <- mapM typeTypeAST types
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject (TIntersection typedTypes) :**: HUnit
  TApplication nameAST args -> do
    typedTypeCons <- typeTypeVariable nameAST
    typedArgs <- mapM typeTypeAST args
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject (TApplication typedTypeCons typedArgs) :**: HUnit

typeTypeVariable :: AST KTypeVariable -> TypingM (TypedAST UnsolvedTy KTypeVariable)
typeTypeVariable ast = do
  let typeVar = hSafeProject @TypeVariable $ extractHFix @Syntax ast
  let meta = extractHFix @Metadata ast
  let typeVarTyInfo = TypeVariableTyInfo @UnsolvedTy
  return $ HFix $ hInject typeVarTyInfo :**: hCoerce meta :**: hInject (hCoerce typeVar) :**: HUnit

getLiteralType :: AST KLiteral -> TypingM UnsolvedTy
getLiteralType ast = case safeProjectVia @Syntax ast of
  NumberLiteral _ -> return $ injectFix Ty.TNumber
  IntLiteral _    -> return $ injectFix Ty.TInt
  BoolLiteral _   -> return $ injectFix Ty.TBool
  StringLiteral _ -> return $ injectFix Ty.TString

-- Expression typing
typeExpression :: AST KExpr -> TypingM (TypedAST UnsolvedTy KExpr)
typeExpression ast = let meta = extractHFix @Metadata ast in case safeProjectVia @Syntax ast of
  EVar varAST -> do
    typedVar <- typeVariable varAST
    let
      -- Extract typedVar's type information
      VariableTyInfo ty = safeProjectVia @(TyInfo UnsolvedTy) typedVar
      -- Expression has same type of var
      exprTyInfo = ExprTyInfo @UnsolvedTy ty
    return
      $ HFix
      $ hInject exprTyInfo
        :**: hCoerce meta
        :**: hInject (EVar typedVar) :**: HUnit

  ELiteral literalAST -> do
    typedLiteral <- typeLiteral literalAST
    literalType <- getLiteralType literalAST
    let exprTyInfo = ExprTyInfo @UnsolvedTy literalType
    return $ HFix $ hInject exprTyInfo :**: hCoerce meta :**: hInject (ELiteral typedLiteral) :**: HUnit

  ELambda params bodyAST -> do
    -- Save current variables
    state <- get
    let savedVariables = tsVariables state
    
    -- Type parameters and introduce their variables
    typedParams <- mapM (\(patAST, _) -> do
      typedPat <- typePattern patAST
      return (typedPat, Nothing)) params
    
    -- Type body with parameter variables in scope
    typedBody <- typeExpression bodyAST
    
    -- Restore original variables
    currentState <- get
    put $ currentState { tsVariables = savedVariables }
    
    paramTypes <- mapM (\(patAST, _) -> getPatternType patAST) typedParams
    let ExprTyInfo @UnsolvedTy bodyType = safeProjectVia @(TyInfo UnsolvedTy) typedBody
    let lambdaType = injectFix $ Ty.TFunction paramTypes bodyType
    let exprTyInfo = ExprTyInfo @UnsolvedTy lambdaType
    return $ HFix $ hInject exprTyInfo :**: hCoerce meta :**: hInject (ELambda typedParams typedBody) :**: HUnit

  EApply funcAST argsAST -> do
    typedFunc <- typeExpression funcAST
    typedArgs <- mapM typeExpression argsAST
    resultType <- freshTyVar
    let exprTyInfo = ExprTyInfo @UnsolvedTy resultType
    return $ HFix $ hInject exprTyInfo :**: hCoerce meta :**: hInject (EApply typedFunc typedArgs) :**: HUnit

  EIf condAST thenAST elseAST -> do
    typedCond <- typeExpression condAST
    typedThen <- typeExpression thenAST
    typedElse <- typeExpression elseAST
    resultType <- freshTyVar
    let exprTyInfo = ExprTyInfo @UnsolvedTy resultType
    return $ HFix $ hInject exprTyInfo :**: hCoerce meta :**: hInject (EIf typedCond typedThen typedElse) :**: HUnit

  EBinOp op leftAST rightAST -> do
    typedLeft <- typeExpression leftAST
    typedRight <- typeExpression rightAST
    resultType <- freshTyVar
    let exprTyInfo = ExprTyInfo @UnsolvedTy resultType
    return $ HFix $ hInject exprTyInfo :**: hCoerce meta :**: hInject (EBinOp op typedLeft typedRight) :**: HUnit

  EMatch scrutineesAST clausesAST -> do
    typedScrutinees <- mapM typeExpression scrutineesAST
    typedClauses <- mapM (\(patternsAndTypes, exprAST) -> do
      -- Save current variables
      state <- get
      let savedVariables = tsVariables state
      
      -- Type patterns and introduce their variables
      typedPatternsAndTypes <- mapM (\(patAST, _) -> do
        typedPat <- typePattern patAST
        return (typedPat, Nothing)) patternsAndTypes
      
      -- Type expression with pattern variables in scope
      typedExpr <- typeExpression exprAST
      
      -- Restore original variables
      currentState <- get
      put $ currentState { tsVariables = savedVariables }
      
      return (typedPatternsAndTypes, typedExpr)) clausesAST
    resultType <- freshTyVar
    let exprTyInfo = ExprTyInfo @UnsolvedTy resultType
    return $ HFix $ hInject exprTyInfo :**: hCoerce meta :**: hInject (EMatch typedScrutinees typedClauses) :**: HUnit

  EBlock blockAST -> do
    typedBlock <- typeBlock blockAST
    let BlockTyInfo blockType = safeProjectVia @(TyInfo UnsolvedTy) typedBlock
    let exprTyInfo = ExprTyInfo @UnsolvedTy blockType
    return $ HFix $ hInject exprTyInfo :**: hCoerce meta :**: hInject (EBlock typedBlock) :**: HUnit

typeBlock :: AST KBlock -> TypingM (TypedAST UnsolvedTy KBlock)
typeBlock ast = case safeProjectVia @Syntax ast of
  Block letsAST exprAST -> do
    let meta = extractHFix @Metadata ast
    -- Let bindings are typed sequentially, each adding variables to the scope
    typedLets <- mapM typeLet letsAST
    typedExpr <- typeExpression exprAST
    let ExprTyInfo @UnsolvedTy exprType = safeProjectVia @(TyInfo UnsolvedTy) typedExpr
    let blockTyInfo = BlockTyInfo @UnsolvedTy exprType
    return $ HFix $ hInject blockTyInfo :**: hCoerce meta :**: hInject (Block typedLets typedExpr) :**: HUnit

typeLet :: AST KLet -> TypingM (TypedAST UnsolvedTy KLet)
typeLet ast = case safeProjectVia @Syntax ast of
  Let patAST _ exprAST -> do
    let meta = extractHFix @Metadata ast
    typedPat <- typePattern patAST
    typedExpr <- typeExpression exprAST
    letType <- freshTyVar
    let letTyInfo = LetTyInfo @UnsolvedTy letType
    return $ HFix $ hInject letTyInfo :**: hCoerce meta :**: hInject (Let typedPat Nothing typedExpr) :**: HUnit

-- Pattern typing
typePattern :: AST KPattern -> TypingM (TypedAST UnsolvedTy KPattern)
typePattern ast = case safeProjectVia @Syntax ast of
  PVar varAST -> do
    let meta = extractHFix @Metadata ast
    typedVar <- introTypeVariable varAST  -- Introduce new variable instead of looking up
    patternType <- freshTyVar
    let patTyInfo = PatternTyInfo @UnsolvedTy patternType
    return $ HFix $ hInject patTyInfo :**: hCoerce meta :**: hInject (PVar typedVar) :**: HUnit

  PWildcard -> do
    let meta = extractHFix @Metadata ast
    patternType <- freshTyVar
    let patTyInfo = PatternTyInfo @UnsolvedTy patternType
    return $ HFix $ hInject patTyInfo :**: hCoerce meta :**: hInject PWildcard :**: HUnit

  PLiteral literalAST -> do
    let meta = extractHFix @Metadata ast
    typedLiteral <- typeLiteral literalAST
    literalType <- getLiteralType literalAST
    let patTyInfo = PatternTyInfo @UnsolvedTy literalType
    return $ HFix $ hInject patTyInfo :**: hCoerce meta :**: hInject (PLiteral typedLiteral) :**: HUnit

  PCons consAST argsAST -> do
    let Var consName = safeProjectVia @Syntax consAST
    state <- get
    case findConstructor consName (tsTypeConstructors state) of
      Nothing -> throwError (UndefinedValueConstructor consName (extractSourcePos consAST))
      Just (TyConsConstructor _ generics _ _) -> do
        let _ = generics
        typedCons <- typeVariable consAST
        typedArgs <- mapM typePattern argsAST
        patternType <- freshTyVar
        let patTyInfo = PatternTyInfo @UnsolvedTy patternType
        let meta = extractHFix @Metadata ast
        return $ HFix $ hInject patTyInfo :**: hCoerce meta :**: hInject (PCons typedCons typedArgs) :**: HUnit

typeLiteral :: AST KLiteral -> TypingM (TypedAST UnsolvedTy KLiteral)
typeLiteral ast =
  let
    lit = safeProjectVia @Syntax ast
    meta = extractHFix @Metadata ast
    literalTyInfo = LiteralTyInfo @UnsolvedTy $ injectFix $ case lit of
      NumberLiteral _ -> Ty.TNumber
      IntLiteral _    -> Ty.TInt
      BoolLiteral _   -> Ty.TBool
      StringLiteral _ -> Ty.TString
  in return
    $ HFix
    $ hInject literalTyInfo
      :**:
      hCoerce meta
      :**:
      hInject (hCoerce lit)
      :**:
      HUnit

getPatternType :: TypedAST UnsolvedTy KPattern -> TypingM UnsolvedTy
getPatternType ast = do
  let PatternTyInfo @UnsolvedTy patType = safeProjectVia @(TyInfo UnsolvedTy) ast
  return patType

findConstructor :: Text -> TyCons (Maybe Variance) -> Maybe TyConsConstructor
findConstructor name tyCons =
  let allConstructors = concatMap snd (Map.elems tyCons)
  in case filter (\c -> tccName c == name) allConstructors of
    []    -> Nothing
    (c:_) -> Just c

-- Definition typing
typeDefinition :: AST KDefinition -> TypingM (TypedAST UnsolvedTy KDefinition)
typeDefinition ast = let meta = extractHFix @Metadata ast in case safeProjectVia @Syntax ast of
  ValDef varAST typeParams typeAST exprAST -> do
    typedVar <- topLevelTypeVariable varAST
    typedTypeParams <- mapM typeTypeVariable typeParams
    
    -- Save current generics and add type parameters
    state <- get
    let savedGenerics = tsTypeGenerics state
    let genericNames = map (\ast -> 
          let TypeVar name = hSafeProject @TypeVariable $ extractHFix @Syntax ast
          in name) typeParams
    put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList genericNames) }
    
    -- Type the type annotation and expression with extended generics
    typedType <- typeTypeAST typeAST
    typedExpr <- typeExpression exprAST
    
    -- Restore original generics
    currentState <- get
    put $ currentState { tsTypeGenerics = savedGenerics }
    
    let defTyInfo = DefinitionTyInfo @UnsolvedTy
    return
      $ HFix
      $ hInject defTyInfo
        :**: hCoerce meta
        :**: hInject (ValDef typedVar typedTypeParams typedType typedExpr)
        :**: HUnit

  FnDef varAST typeParams args retTypeAST blockAST -> do
    typedVar <- topLevelTypeVariable varAST
    typedTypeParams <- mapM typeTypeVariable typeParams
    
    -- Save current state (both generics and variables)
    state <- get
    let savedGenerics = tsTypeGenerics state
    let savedVariables = tsVariables state
    
    -- Add type parameters to generics
    let genericNames = map (\ast -> 
          let TypeVar name = hSafeProject @TypeVariable $ extractHFix @Syntax ast
          in name) typeParams
    put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList genericNames) }
    
    -- Type arguments and add them to variable scope
    typedArgs <- mapM (\(argVar, argType) -> do
      tVar <- introTypeVariable argVar
      tType <- typeTypeAST argType
      return (tVar, tType)) args
    
    -- Type return type and body with extended generics and argument variables
    typedRetType <- typeTypeAST retTypeAST
    typedBlock <- typeBlock blockAST
    
    -- Restore original state
    currentState <- get
    put $ currentState { tsTypeGenerics = savedGenerics, tsVariables = savedVariables }
    
    let defTyInfo = DefinitionTyInfo @UnsolvedTy
    return
      $ HFix
      $ hInject defTyInfo
        :**: hCoerce meta
        :**: hInject (FnDef typedVar typedTypeParams typedArgs typedRetType typedBlock)
        :**: HUnit

  DataDef varAST variances constructors -> do
    typedVar <- typeTypeConstructorDeclaration varAST  -- Use helper instead of typeVariable
    -- In DataDef, variances are just variance annotations.
    -- Type parameters are introduced by each constructor independently.
    typedConstructors <- mapM typeConstructorDef constructors
    let defTyInfo = DefinitionTyInfo @UnsolvedTy
    return $ HFix $ hInject defTyInfo :**: hCoerce meta :**: hInject (DataDef typedVar variances typedConstructors) :**: HUnit

  TypeDef varAST typeParams typeAST -> do
    typedVar <- typeTypeConstructorDeclaration varAST  -- Use helper instead of typeVariable
    typedTypeParams <- mapM typeTypeVariable typeParams
    
    -- Save current generics and add type parameters
    state <- get
    let savedGenerics = tsTypeGenerics state
    let genericNames = map (\ast -> 
          let TypeVar name = hSafeProject @TypeVariable $ extractHFix @Syntax ast
          in name) typeParams
    put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList genericNames) }
    
    -- Type the type body with extended generics
    typedType <- typeTypeAST typeAST
    
    -- Restore original generics
    currentState <- get
    put $ currentState { tsTypeGenerics = savedGenerics }
    
    let defTyInfo = DefinitionTyInfo @UnsolvedTy
    return $ HFix $ hInject defTyInfo :**: hCoerce meta :**: hInject (TypeDef typedVar typedTypeParams typedType) :**: HUnit

typeConstructorDef :: ConstructorDef AST -> TypingM (ConstructorDef (TypedAST UnsolvedTy))
typeConstructorDef (ConstructorDef nameAST typeParams args retTypeAST) = do
  -- Save current generics
  state <- get
  let savedGenerics = tsTypeGenerics state
  
  -- Add constructor's type parameters to generics
  let genericNames = map (\ast -> 
        let TypeVar name = hSafeProject @TypeVariable $ extractHFix @Syntax ast
        in name) typeParams
  put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList genericNames) }
  
  -- Type constructor components with extended generics
  typedName <- typeTypeConstructorDeclaration nameAST  -- Constructor name is a declaration
  typedTypeParams <- mapM typeTypeVariable typeParams
  typedArgs <- mapM (\(argVar, argType) -> do
    tVar <- typeFunctionParameter argVar  -- Constructor arguments are like function parameters
    tType <- typeTypeAST argType
    return (tVar, tType)) args
  typedRetType <- typeTypeAST retTypeAST
  
  -- Restore original generics
  currentState <- get
  put $ currentState { tsTypeGenerics = savedGenerics }
  
  return $ ConstructorDef typedName typedTypeParams typedArgs typedRetType

typeVariable :: AST KVariable -> TypingM (TypedAST UnsolvedTy KVariable)
typeVariable varAST = do
  st <- get
  let
    var@(Var name) = safeProjectVia @Syntax varAST
    valDefs = tsValueDefinitions st
    variables = tsVariables st
  -- First check local variables (function parameters, let bindings, etc.)
  case Map.lookup name variables of
    Just ty -> do
      let
        meta = extractHFix @Metadata varAST
        varTyInfo = VariableTyInfo @UnsolvedTy ty
      return
        $ HFix
        $ hInject varTyInfo
          :**: hCoerce meta
          :**: hInject (hCoerce var)
          :**: HUnit
    Nothing -> 
      -- If not found in local variables, check global definitions
      case Map.lookup name valDefs of
        Nothing -> throwError $ UndefinedVariable name (extractSourcePos varAST)
        Just scheme -> do
          inst <- instantiateScheme scheme
          let
            meta = extractHFix @Metadata varAST
            varTyInfo = VariableTyInfo @UnsolvedTy inst
          return
            $ HFix
            $ hInject varTyInfo
              :**: hCoerce meta
              :**: hInject (hCoerce var)
              :**: HUnit

introTypeVariable :: AST KVariable -> TypingM (TypedAST UnsolvedTy KVariable)
introTypeVariable varAST = do
  st <- get
  let
    var@(Var name) = safeProjectVia @Syntax varAST
  freshVar <- freshTyVar
  let
    currentVars = tsVariables st
    newVars = Map.insert name freshVar currentVars
  put $ st { tsVariables = newVars }
  let
    meta = extractHFix @Metadata varAST
    varTyInfo = VariableTyInfo @UnsolvedTy freshVar
  return
    $ HFix
    $ hInject varTyInfo
      :**: hCoerce meta
      :**: hInject (hCoerce var)
      :**: HUnit

topLevelTypeVariable :: AST KVariable -> TypingM (TypedAST UnsolvedTy KVariable)
topLevelTypeVariable varAST = do
  st <- get
  let
    var@(Var name) = safeProjectVia @Syntax varAST
    valDefs = tsValueDefinitions st
  case Map.lookup name valDefs of
    Nothing -> throwError $ UndefinedVariable name (extractSourcePos varAST)
    Just (TypeScheme _ varTy) -> do
      let
        meta = extractHFix @Metadata varAST
        varTyInfo = VariableTyInfo @UnsolvedTy $ tyToUnsolvedTy varTy
      return
        $ HFix
        $ hInject varTyInfo
          :**: hCoerce meta
          :**: hInject (hCoerce var)
          :**: HUnit

-- Create a TypedAST Variable for function type parameter names (not looked up in environment)
typeFunctionParameter :: AST KVariable -> TypingM (TypedAST UnsolvedTy KVariable)
typeFunctionParameter varAST = do
  let var@(Var _) = safeProjectVia @Syntax varAST
  let meta = extractHFix @Metadata varAST
  -- Function type parameters don't have a specific type - they're just names
  dummyType <- freshTyVar
  let varTyInfo = VariableTyInfo @UnsolvedTy dummyType
  return
    $ HFix
    $ hInject varTyInfo
      :**: hCoerce meta
      :**: hInject (hCoerce var)
      :**: HUnit

-- Create a TypedAST Variable for type constructor declarations (not looked up in environment)
typeTypeConstructorDeclaration :: AST KVariable -> TypingM (TypedAST UnsolvedTy KVariable)
typeTypeConstructorDeclaration varAST = do
  let var@(Var _) = safeProjectVia @Syntax varAST
  let meta = extractHFix @Metadata varAST
  -- Type constructor declarations don't have a specific type - they're type-level names
  dummyType <- freshTyVar
  let varTyInfo = VariableTyInfo @UnsolvedTy dummyType
  return
    $ HFix
    $ hInject varTyInfo
      :**: hCoerce meta
      :**: hInject (hCoerce var)
      :**: HUnit
