{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Memento.Typing.Core (
  TypingError(..),
  TypingState(..),
  TypingM,
  freshTyVar,
  freshGeneric,
  syntaxVarianceToVariance,
  extractSourcePos,
  extractSourcePosRange
) where

import           Control.Monad.Except                            (ExceptT)
import           Control.Monad.State                             (State, get,
                                                                  put)
import qualified Data.Map                                        as Map
import qualified Data.Set                                        as Set
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import           Language.Memento.Data.AST                       (AST)
import           Language.Memento.Data.AST.Definition            (SyntaxVariance (..))
import           Language.Memento.Data.AST.Metadata              (Metadata (..))
import           Language.Memento.Data.Environment.Ty            (TyCons)
import           Language.Memento.Data.Environment.Val           (ValDefs)
import           Language.Memento.Data.Environment.Variance      (Variance (..))
import           Language.Memento.Data.Functor.FixedPoint        (injectFix)
import           Language.Memento.Data.Functor.FixedPoint.Higher (extractHFix)
import           Language.Memento.Data.Ty                        (TyVarF (TyVar),
                                                                  UnsolvedTy)
import qualified Language.Memento.Data.Ty                        as Ty
import           Text.Megaparsec                                 (SourcePos,
                                                                  sourceColumn,
                                                                  sourceLine,
                                                                  sourceName)
import           Text.Megaparsec.Pos                             (unPos)

data TypingError
  = UndefinedTypeConstructor Text (Maybe (SourcePos, SourcePos))
  | UndefinedValueConstructor Text (Maybe (SourcePos, SourcePos))
  | UndefinedVariable Text (Maybe (SourcePos, SourcePos))
  | ArityMismatch Text Int Int (Maybe (SourcePos, SourcePos)) -- name, expected, actual
  | TypeVariableNotInScope Text (Maybe (SourcePos, SourcePos))
  deriving (Eq)

instance Show TypingError where
  show = \case
    UndefinedTypeConstructor name Nothing -> "UndefinedTypeConstructor " ++ T.unpack name
    UndefinedTypeConstructor name (Just (startPos, _)) -> "UndefinedTypeConstructor " ++ T.unpack name ++ " at " ++ showSourcePos startPos
    UndefinedValueConstructor name Nothing -> "UndefinedValueConstructor " ++ T.unpack name
    UndefinedValueConstructor name (Just (startPos, _)) -> "UndefinedValueConstructor " ++ T.unpack name ++ " at " ++ showSourcePos startPos
    UndefinedVariable name Nothing -> "UndefinedVariable " ++ T.unpack name
    UndefinedVariable name (Just (startPos, _)) -> "UndefinedVariable " ++ T.unpack name ++ " at " ++ showSourcePos startPos
    ArityMismatch name expected actual Nothing -> "ArityMismatch " ++ T.unpack name ++ " expected " ++ show expected ++ " but got " ++ show actual
    ArityMismatch name expected actual (Just (startPos, _)) -> "ArityMismatch " ++ T.unpack name ++ " expected " ++ show expected ++ " but got " ++ show actual ++ " at " ++ showSourcePos startPos
    TypeVariableNotInScope name Nothing -> "TypeVariableNotInScope " ++ T.unpack name
    TypeVariableNotInScope name (Just (startPos, _)) -> "TypeVariableNotInScope " ++ T.unpack name ++ " at " ++ showSourcePos startPos
    where
      showSourcePos pos = T.unpack (T.pack (sourceName pos)) ++ ":" ++ show (unPos (sourceLine pos)) ++ ":" ++ show (unPos (sourceColumn pos))

data TypingState = TypingState
  { -- All variances are annotated with `Maybe Variance` (if annotated then Just v, if "auto", then Nothing)
    -- This will be solved by `TypeSolver.SolveVariances`
    tsTypeConstructors :: TyCons (Maybe Variance)
  , tsTypeGenerics     :: Set.Set Text
  , tsValueDefinitions :: ValDefs
  , tsVariables        :: Map.Map Text UnsolvedTy
  , tsNextVarId        :: Int
  }

type TypingM a = ExceptT TypingError (State TypingState) a

-- Generate fresh type variable
freshTyVar :: TypingM UnsolvedTy
freshTyVar = do
  state <- get
  let varId = tsNextVarId state
  put $ state { tsNextVarId = varId + 1 }
  return $ injectFix $ TyVar $ "t" <> T.pack (show varId)

-- Generate fresh generic variable
freshGeneric :: TypingM UnsolvedTy
freshGeneric = do
  state <- get
  let genId = tsNextVarId state
  put $ state { tsNextVarId = genId + 1 }
  return $ injectFix $ Ty.TGeneric $ "#T" <> T.pack (show genId)

syntaxVarianceToVariance :: SyntaxVariance -> Maybe Variance
syntaxVarianceToVariance = \case
  SVAuto -> Nothing
  SVIn -> Just Contravariant
  SVOut -> Just Covariant
  SVInOut -> Just Invariant
  SVPhantom -> Just Bivariant

-- Extract source position from AST node
extractSourcePos :: AST k -> Maybe SourcePos
extractSourcePos ast = case extractHFix ast of
  Metadata startPos _ -> Just startPos

-- Extract source position range from AST node
extractSourcePosRange :: AST k -> Maybe (SourcePos, SourcePos)
extractSourcePosRange ast = case extractHFix ast of
  Metadata startPos endPos -> Just (startPos, endPos)
