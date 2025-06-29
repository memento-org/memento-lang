{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Memento.Typing.Collection (
  collectDefinition,
  collectConstructor
) where

import           Control.Monad.State                             (get, modify, put)
import qualified Data.Map                                        as Map
import qualified Data.Set                                        as Set
import           Language.Memento.Data.AST                       (AST, Syntax)
import           Language.Memento.Data.AST.Definition            (ConstructorDef (ConstructorDef),
                                                                  Definition (DataDef, FnDef, TypeDef, ValDef))
import           Language.Memento.Data.AST.Tag                   (KDefinition,
                                                                  KType)
import           Language.Memento.Data.AST.Variable              (TypeVariable (TypeVar),
                                                                  Variable (Var))
import           Language.Memento.Data.Environment.Ty            (TyConsConstructor (..))
import           Language.Memento.Data.Functor.FixedPoint        (injectFix,
                                                                  projectFix)
import           Language.Memento.Data.Functor.FixedPoint.Higher (safeProjectVia)
import           Language.Memento.Data.Ty                        (Ty,
                                                                  TypeScheme (..))
import qualified Language.Memento.Data.Ty                        as Ty
import           Language.Memento.Typing.Core                    (TypingM,
                                                                  TypingState (..),
                                                                  syntaxVarianceToVariance)

-- Phase 1: Collection functions
collectDefinition :: (AST KType -> TypingM Ty) -> AST KDefinition -> TypingM ()
collectDefinition typeTypeFn ast = case safeProjectVia @Syntax ast of
  ValDef varAST typeParams typeAST _ -> do
    let
      Var name = safeProjectVia @Syntax varAST
      paramNames = map (\tpAST -> let TypeVar pname = safeProjectVia @Syntax tpAST in pname) typeParams
    
    -- Save current generics and add type parameters for typing the type annotation
    state <- get
    let savedGenerics = tsTypeGenerics state
    put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList paramNames) }
    
    -- Type the type annotation with type parameters in scope
    typedType <- typeTypeFn typeAST
    
    -- Restore original generics
    currentState <- get
    put $ currentState { tsTypeGenerics = savedGenerics }
    
    let scheme = TypeScheme paramNames typedType
    modify $ \s -> s { tsValueDefinitions = Map.insert name scheme (tsValueDefinitions s) }

  FnDef varAST typeParams args retTypeAST _ -> do
    let
      Var name = safeProjectVia @Syntax varAST
      paramNames = map (\tpAST -> let TypeVar pname = safeProjectVia @Syntax tpAST in pname) typeParams
    
    -- Save current generics and add type parameters for typing function signature
    state <- get
    let savedGenerics = tsTypeGenerics state
    put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList paramNames) }
    
    -- Type arguments and return type with type parameters in scope
    typedArgs <- mapM (\(_, argType) -> typeTypeFn argType) args
    typedRetType :: Ty <- typeTypeFn retTypeAST
    
    -- Restore original generics
    currentState <- get
    put $ currentState { tsTypeGenerics = savedGenerics }
    
    let scheme = TypeScheme paramNames $ injectFix $ Ty.TFunction typedArgs typedRetType
    modify $ \s -> s { tsValueDefinitions = Map.insert name scheme (tsValueDefinitions s) }

  DataDef varAST variances constructors -> do
    let
      Var name = safeProjectVia @Syntax varAST
      variances' = map syntaxVarianceToVariance variances
    
    -- Add type constructor to state first (with empty constructors) so constructors can reference it
    modify $ \s -> s { tsTypeConstructors = Map.insert name (variances', []) (tsTypeConstructors s) }
    
    -- Now collect constructors with type constructor available
    tyConsConstructors <- mapM (collectConstructor typeTypeFn) constructors
    
    -- Add constructors to value definitions so they can be called as functions
    mapM_ (addConstructorToValDefs typeTypeFn) constructors
    
    -- Update with complete constructor information
    modify $ \s -> s { tsTypeConstructors = Map.insert name (variances', tyConsConstructors) (tsTypeConstructors s) }

  TypeDef {} -> do
    -- TODO: Handle type definitions
    return ()

collectConstructor :: (AST KType -> TypingM Ty) -> ConstructorDef AST -> TypingM TyConsConstructor
collectConstructor typeTypeFn (ConstructorDef nameAST typeParams args retTypeAST) = do
  let
    Var name = safeProjectVia @Syntax nameAST
    paramNames = map (\tpAST -> let TypeVar pname = safeProjectVia @Syntax tpAST in pname) typeParams
  
  -- Save current generics and add constructor type parameters
  state <- get
  let savedGenerics = tsTypeGenerics state
  put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList paramNames) }
  
  -- Type constructor components with extended generics
  argTypes <- mapM (\(_, argTypeAST) -> typeTypeFn argTypeAST) args
  retType <- typeTypeFn retTypeAST
  
  -- Restore original generics
  currentState <- get
  put $ currentState { tsTypeGenerics = savedGenerics }
  
  -- Extract type arguments from return type
  retTypeArgs <- extractTypeArgs retType
  return $ TyConsConstructor { tccName = name, tccGenerics = paramNames, tccArgs = argTypes, tccReturn = retTypeArgs }
  where
    extractTypeArgs :: Ty -> TypingM [Ty]
    extractTypeArgs ty = case projectFix ty of
      Just (Ty.TApplication _ tcArgs) -> return tcArgs
      _                               -> return []

-- Add constructor to value definitions so it can be called as a function
addConstructorToValDefs :: (AST KType -> TypingM Ty) -> ConstructorDef AST -> TypingM ()
addConstructorToValDefs typeTypeFn (ConstructorDef nameAST typeParams args retTypeAST) = do
  let
    Var name = safeProjectVia @Syntax nameAST
    paramNames = map (\tpAST -> let TypeVar pname = safeProjectVia @Syntax tpAST in pname) typeParams
  
  -- Save current generics and add constructor type parameters
  state <- get
  let savedGenerics = tsTypeGenerics state
  put $ state { tsTypeGenerics = Set.union savedGenerics (Set.fromList paramNames) }
  
  -- Type constructor signature
  argTypes <- mapM (\(_, argType) -> typeTypeFn argType) args
  retType <- typeTypeFn retTypeAST
  
  -- Restore original generics
  currentState <- get
  put $ currentState { tsTypeGenerics = savedGenerics }
  
  -- Create function type for constructor
  let constructorType = injectFix $ Ty.TFunction argTypes retType
  let scheme = TypeScheme paramNames constructorType
  
  -- Add to value definitions
  modify $ \s -> s { tsValueDefinitions = Map.insert name scheme (tsValueDefinitions s) }
