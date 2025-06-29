{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Memento.Typing (
  typeProgram,
  typeProgramWithTyCons,
  TypingError(..)
) where

import           Control.Monad.Except                            (runExceptT)
import           Control.Monad.State                             (runState)
import qualified Data.Map                                        as Map
import qualified Data.Set                                        as Set
import qualified Data.Text                                       as T
import           Language.Memento.Data.AST                       (AST, Syntax)
import           Language.Memento.Data.AST.Metadata              (Metadata)
import           Language.Memento.Data.AST.Program               (Program (Program))
import           Language.Memento.Data.AST.Tag                   (KProgram)
import           Language.Memento.Data.Environment.Ty            (TyConsConstructor)
import           Language.Memento.Data.Environment.Variance      (Variance)
import           Language.Memento.Data.Functor.Coproduct.Higher  (HInjective (hInject))
import           Language.Memento.Data.Functor.FixedPoint.Higher (HFix (..),
                                                                  extractHFix,
                                                                  safeProjectVia)
import           Language.Memento.Data.Functor.Higher            (HPhantom (hCoerce))
import           Language.Memento.Data.Functor.Product.Higher    (HUnit (..),
                                                                  (:**:) (..))
import           Language.Memento.Data.Ty                        (UnsolvedTy)
import           Language.Memento.Data.TypedAST                  (TypedAST)
import           Language.Memento.Data.TypedAST.TyInfo           (ProgramTyInfo (ProgramTyInfo))

-- Import the typing modules
import qualified Language.Memento.Typing.Collection              as Collection
import           Language.Memento.Typing.Core                    (TypingError (..),
                                                                  TypingM,
                                                                  TypingState (..))
import qualified Language.Memento.Typing.Inference               as Inference

{-
This module defines transformation `AST -> TypedAST UnsolvedTy`.
`TypedAST UnsolvedTy` will be solved by `TypeSolver`.
-}

-- Main entry point for typing
typeProgram :: AST KProgram -> Either TypingError (TypedAST UnsolvedTy KProgram)
typeProgram ast = fst <$> typeProgramWithTyCons ast

-- Type program and also return type constructors for variance solving
typeProgramWithTyCons :: AST KProgram -> Either TypingError (TypedAST UnsolvedTy KProgram, Map.Map T.Text ([Maybe Variance], [TyConsConstructor]))
typeProgramWithTyCons ast =
  let
    initialState = TypingState
      { tsTypeConstructors = Map.empty
      , tsTypeGenerics = Set.empty
      , tsValueDefinitions = Map.empty
      , tsVariables = Map.empty
      , tsNextVarId = 0 }
    result = runState (runExceptT (typeProgramM ast)) initialState
  in case result of
    (Left err, _)          -> Left err
    (Right typedAST, finalState) -> Right (typedAST, tsTypeConstructors finalState)

typeProgramM :: AST KProgram -> TypingM (TypedAST UnsolvedTy KProgram)
typeProgramM ast = case safeProjectVia @Syntax ast of
  Program definitions -> do
    -- Phase 1: Collect type constructors and value definitions
    mapM_ (Collection.collectDefinition Inference.synTypeToTy) definitions

    -- Phase 2: Type the definitions
    typedDefinitions <- mapM Inference.typeDefinition definitions

    -- Create program type info
    let programTyInfo = ProgramTyInfo @UnsolvedTy

    let meta = extractHFix @Metadata ast
    return
      $ HFix
      $ hInject programTyInfo
        :**: hCoerce meta
        :**: hInject (Program typedDefinitions)
        :**: HUnit
