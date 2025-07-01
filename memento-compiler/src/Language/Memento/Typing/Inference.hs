{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Memento.Typing.Inference (
  -- Type inference functions
  typeType,
  synTypeToTy,
  typeTypeVariable,
  getLiteralType,

  -- Expression typing
  typeExpression,
  typeBlock,
  typeLet,

  -- Pattern typing
  typeLiteral,
  getPatternType,
  findConstructor,

  -- Definition typing
  typeDefinition,
  typeConstructorDef,
  typeVariable,
  typeIntroVariable,
  typeTopLevelVariable
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
                                                                  extractSourcePosRange,
                                                                  freshGeneric,
                                                                  freshTyVar)
import           Language.Memento.Typing.Scheme                  (instantiateScheme,
                                                                  instantiateSchemeWithGenerics)

-- Type operations
synTypeToTy :: AST KType -> TypingM Ty
synTypeToTy ast = case safeProjectVia @Syntax ast of
  TVar typeVarAST -> do
    let TypeVar name = safeProjectVia @Syntax typeVarAST
    state <- get
    if Set.member name (tsTypeGenerics state) -- Is Generic?
      then return $ injectFix $ Ty.TGeneric name
      else case  Map.lookup name (tsTypeConstructors state) of -- Is a type constructor with nullary?
        Just (variances, _)  ->
          let actualArity = length variances
          in if actualArity == 0 then
            return $ injectFix $ Ty.TApplication name []
          else
            throwError (ArityMismatch name 0 actualArity (extractSourcePosRange typeVarAST))
        _ -> throwError (TypeVariableNotInScope name (extractSourcePosRange typeVarAST))
  TNumber -> return $ injectFix Ty.TNumber
  TInt -> return $ injectFix Ty.TInt
  TBool -> return $ injectFix Ty.TBool
  TString -> return $ injectFix Ty.TString
  TFunction argVarTypes retType -> do
    typedArgTypes <- mapM (\(_, argTypeAST) -> synTypeToTy argTypeAST) argVarTypes
    typedRetType <- synTypeToTy retType
    return $ injectFix $ Ty.TFunction typedArgTypes typedRetType
  TUnknown -> return $ injectFix Ty.TUnknown
  TNever -> return $ injectFix Ty.TNever
  TLiteral literalAST -> do
    let lit = safeProjectVia @Syntax literalAST
    return $ injectFix $ case lit of
      NumberLiteral n -> Ty.TLiteral $ Ty.LNumber n
      IntLiteral n    -> Ty.TLiteral $ Ty.LInt n
      BoolLiteral b   -> Ty.TLiteral $ Ty.LBool b
      StringLiteral s -> Ty.TLiteral $ Ty.LString s
  TUnion types -> do
    typedTypes <- mapM synTypeToTy types
    return $ injectFix $ Ty.TUnion typedTypes
  TIntersection types -> do
    typedTypes <- mapM synTypeToTy types
    return $ injectFix $ Ty.TIntersection typedTypes
  TApplication nameAST args -> do
    let TypeVar name = safeProjectVia @Syntax nameAST
    state <- get
    case Map.lookup name (tsTypeConstructors state) of
      Nothing -> throwError (UndefinedTypeConstructor name (extractSourcePosRange nameAST))
      Just (variances, _) -> do
        let expectedArity = length variances
        let actualArity = length args
        if expectedArity /= actualArity
          then throwError (ArityMismatch name expectedArity actualArity (extractSourcePosRange nameAST))
          else do
            typedArgs <- mapM synTypeToTy args
            return $ injectFix $ Ty.TApplication name typedArgs

typeType :: AST KType -> TypingM (TypedAST UnsolvedTy KType)
typeType ast = let meta = extractHFix @Metadata ast in case safeProjectVia @Syntax ast of
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
      ty <- synTypeToTy typeAST  -- Convert AST type to UnsolvedTy
      typedVar <- typeVariableWith varAST $ tyToUnsolvedTy ty
      typedType <- typeType typeAST
      return (typedVar, typedType)) params
    typedRetType <- typeType retType
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
    typedTypes <- mapM typeType types
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject (TUnion typedTypes) :**: HUnit
  TIntersection types -> do
    typedTypes <- mapM typeType types
    let typeTyInfo = TypeTyInfo @UnsolvedTy
    return $ HFix $ hInject typeTyInfo :**: hCoerce meta :**: hInject (TIntersection typedTypes) :**: HUnit
  TApplication nameAST args -> do
    typedTypeCons <- typeTypeVariable nameAST
    typedArgs <- mapM typeType args
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
  NumberLiteral n -> return $ injectFix $ Ty.TLiteral $ Ty.LNumber n
  IntLiteral n    -> return $ injectFix $ Ty.TLiteral $ Ty.LInt n
  BoolLiteral b   -> return $ injectFix $ Ty.TLiteral $ Ty.LBool b
  StringLiteral s -> return $ injectFix $ Ty.TLiteral $ Ty.LString s

-- Convert optional type annotation to UnsolvedTy
maybeTypeToUnsolvedTy :: Maybe (AST KType) -> TypingM (Maybe UnsolvedTy)
maybeTypeToUnsolvedTy Nothing = return Nothing
maybeTypeToUnsolvedTy (Just typeAST) = do
  ty <- synTypeToTy typeAST
  return $ Just $ tyToUnsolvedTy ty

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
    typedParams <- mapM (\(patAST, maybeTypeAST) -> do
      maybeExpectedType <- maybeTypeToUnsolvedTy maybeTypeAST
      expectedType <- maybe freshTyVar return maybeExpectedType
      typedPat <- typePatternWithType patAST $ Just expectedType
      typedType <- case maybeTypeAST of
        Nothing      -> return Nothing
        Just typeAST -> Just <$> typeType typeAST
      return (typedPat, typedType)) params

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
      typedPatternsAndTypes <- mapM (\(patAST, maybeTypeAST) -> do
        maybeExpectedType <- maybeTypeToUnsolvedTy maybeTypeAST
        expectedType <- maybe freshTyVar return maybeExpectedType
        typedPat <- typePatternWithType patAST $ Just expectedType
        typedType <- case maybeTypeAST of
          Nothing      -> return Nothing
          Just typeAST -> Just <$> typeType typeAST
        return (typedPat, typedType)) patternsAndTypes

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
  Let patAST maybeTypeAST exprAST -> do
    let meta = extractHFix @Metadata ast
    maybeExpectedType <- maybeTypeToUnsolvedTy maybeTypeAST
    expectedType <- maybe freshTyVar return maybeExpectedType
    typedPat <- typePatternWithType patAST $ Just expectedType
    typedExpr <- typeExpression exprAST
    typedType <- case maybeTypeAST of
      Nothing      -> return Nothing
      Just typeAST -> Just <$> typeType typeAST
    letType <- freshTyVar
    let letTyInfo = LetTyInfo @UnsolvedTy letType
    return $ HFix $ hInject letTyInfo :**: hCoerce meta :**: hInject (Let typedPat typedType typedExpr) :**: HUnit

-- Pattern typing with optional expected type
typePatternWithType :: AST KPattern -> Maybe UnsolvedTy -> TypingM (TypedAST UnsolvedTy KPattern)
typePatternWithType ast maybeExpectedType = case safeProjectVia @Syntax ast of
  PVar varAST -> do
    let meta = extractHFix @Metadata ast
    patternType <- maybe freshGeneric return maybeExpectedType
    typedVar <- typeIntroVariable varAST patternType  -- Introduce new variable instead of looking up
    let patTyInfo = PatternTyInfo @UnsolvedTy patternType
    return $ HFix $ hInject patTyInfo :**: hCoerce meta :**: hInject (PVar typedVar) :**: HUnit

  PWildcard -> do
    let meta = extractHFix @Metadata ast
    patternType <- maybe freshGeneric return maybeExpectedType
    let patTyInfo = PatternTyInfo @UnsolvedTy patternType
    return $ HFix $ hInject patTyInfo :**: hCoerce meta :**: hInject PWildcard :**: HUnit

  PLiteral literalAST -> do
    let meta = extractHFix @Metadata ast
    typedLiteral <- typeLiteral literalAST
    patternType <- maybe freshGeneric return maybeExpectedType
    let patTyInfo = PatternTyInfo @UnsolvedTy patternType
    return $ HFix $ hInject patTyInfo :**: hCoerce meta :**: hInject (PLiteral typedLiteral) :**: HUnit

  PCons consAST argsAST -> do
    let Var consName = safeProjectVia @Syntax consAST
    state <- get
    case findConstructor consName (tsTypeConstructors state) of
      Nothing -> throwError (UndefinedValueConstructor consName (extractSourcePosRange consAST))
      Just (TyConsConstructor _ generics _ _) -> do
        let _ = generics
        typedCons <- typeConsPatternVariable consAST
        typedArgs <- mapM (`typePatternWithType` Nothing) argsAST  -- Use typePatternWithType for consistency
        patternType <- maybe freshGeneric return maybeExpectedType
        let patTyInfo = PatternTyInfo @UnsolvedTy patternType
        let meta = extractHFix @Metadata ast
        return $ HFix $ hInject patTyInfo :**: hCoerce meta :**: hInject (PCons typedCons typedArgs) :**: HUnit


typeLiteral :: AST KLiteral -> TypingM (TypedAST UnsolvedTy KLiteral)
typeLiteral ast =
  let
    lit = safeProjectVia @Syntax ast
    meta = extractHFix @Metadata ast
    literalTyInfo = LiteralTyInfo @UnsolvedTy $ injectFix $ case lit of
      NumberLiteral n -> Ty.TLiteral $ Ty.LNumber n
      IntLiteral n    -> Ty.TLiteral $ Ty.LInt n
      BoolLiteral b   -> Ty.TLiteral $ Ty.LBool b
      StringLiteral s -> Ty.TLiteral $ Ty.LString s
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
    typedVar <- typeTopLevelVariable varAST
    typedTypeParams <- mapM typeTypeVariable typeParams

    -- Save current generics and add type parameters
    state <- get
    let savedGenerics = tsTypeGenerics state
    let genericNames = map (\typeVarAst ->
          let TypeVar name = safeProjectVia @Syntax typeVarAst
          in name) typeParams
    put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList genericNames) }

    -- Type the type annotation and expression with extended generics
    typedType <- typeType typeAST
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
    typedVar <- typeTopLevelVariable varAST
    typedTypeParams <- mapM typeTypeVariable typeParams

    -- Save current state (both generics and variables)
    state <- get
    let savedGenerics = tsTypeGenerics state
    let savedVariables = tsVariables state

    -- Add type parameters to generics
    let genericNames = map (\typeVarAst ->
          let TypeVar name = safeProjectVia @Syntax typeVarAst
          in name) typeParams
    put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList genericNames) }

    -- Type arguments and add them to variable scope
    typedArgs <- mapM (\(argVar, argType) -> do
      tType <- typeType argType
      ty <- synTypeToTy argType  -- Convert AST type to UnsolvedTy
      tVar <- typeIntroVariable argVar $ tyToUnsolvedTy ty
      return (tVar, tType)) args

    -- Type return type and body with extended generics and argument variables
    typedRetType <- typeType retTypeAST
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
    let genericNames = map (\typeVarAst ->
          let TypeVar name = safeProjectVia @Syntax typeVarAst
          in name) typeParams
    put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList genericNames) }

    -- Type the type body with extended generics
    typedType <- typeType typeAST

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
        let TypeVar name = safeProjectVia @Syntax ast
        in name) typeParams
  put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList genericNames) }

  -- Type constructor components with extended generics
  typedName <- typeTypeConstructorDeclaration nameAST  -- Constructor name is a declaration
  typedTypeParams <- mapM typeTypeVariable typeParams
  typedArgs <- mapM (\(argVar, argType) -> do
    ty <- synTypeToTy argType  -- Convert AST type to UnsolvedTy
    tVar <- typeVariableWith argVar $ tyToUnsolvedTy ty  -- Constructor arguments are like function parameters
    tType <- typeType argType
    return (tVar, tType)) args
  typedRetType <- typeType retTypeAST

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
        Nothing -> throwError $ UndefinedVariable name (extractSourcePosRange varAST)
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

typeConsPatternVariable :: AST KVariable -> TypingM (TypedAST UnsolvedTy KVariable)
typeConsPatternVariable varAST = do
  st <- get
  let
    var@(Var name) = safeProjectVia @Syntax varAST
    valDefs = tsValueDefinitions st
  -- Only check constructors
  case Map.lookup name valDefs of
      Nothing -> throwError $ UndefinedVariable name (extractSourcePosRange varAST)
      Just scheme -> do
        inst <- instantiateSchemeWithGenerics scheme
        let
          meta = extractHFix @Metadata varAST
          varTyInfo = VariableTyInfo @UnsolvedTy inst
        return
          $ HFix
          $ hInject varTyInfo
            :**: hCoerce meta
            :**: hInject (hCoerce var)
            :**: HUnit

typeIntroVariable :: AST KVariable -> UnsolvedTy ->  TypingM (TypedAST UnsolvedTy KVariable)
typeIntroVariable varAST uty = do
  let
    var@(Var name) = safeProjectVia @Syntax varAST
  st <- get  -- Get the current state AFTER freshTyVar
  let
    currentVars = tsVariables st
    newVars = Map.insert name uty currentVars
  put $ st { tsVariables = newVars }
  let
    meta = extractHFix @Metadata varAST
    varTyInfo = VariableTyInfo @UnsolvedTy uty
  return
    $ HFix
    $ hInject varTyInfo
      :**: hCoerce meta
      :**: hInject (hCoerce var)
      :**: HUnit

typeTopLevelVariable :: AST KVariable -> TypingM (TypedAST UnsolvedTy KVariable)
typeTopLevelVariable varAST = do
  st <- get
  let
    var@(Var name) = safeProjectVia @Syntax varAST
    valDefs = tsValueDefinitions st
  case Map.lookup name valDefs of
    Nothing -> throwError $ UndefinedVariable name (extractSourcePosRange varAST)
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
typeVariableWith :: AST KVariable -> UnsolvedTy -> TypingM (TypedAST UnsolvedTy KVariable)
typeVariableWith varAST ty = do
  let var@(Var _) = safeProjectVia @Syntax varAST
  let meta = extractHFix @Metadata varAST
  -- Function type parameters don't have a specific type - they're just names
  let varTyInfo = VariableTyInfo @UnsolvedTy ty
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
