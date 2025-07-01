{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards   #-}

{-# LANGUAGE DisambiguateRecordFields #-}

module Language.Memento.LSP.Diagnostics
  ( computeDiagnostics
  ) where

import           Data.List.NonEmpty                         (NonEmpty (..))
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Data.Void                                  (Void)
import           Language.LSP.Protocol.Types
import           Language.Memento.Data.AST.Tag              (KProgram)
import           Language.Memento.Parser                    (parseAST)
import           Language.Memento.TypeSolver                (SolveError (..),
                                                             solveTypedAST)
import           Language.Memento.TypeSolver.SolveVariances (VarianceError (..),
                                                             checkVarianceConsistency,
                                                             solveVariancesFromEnv)
import           Language.Memento.Typing                    (typeProgramWithTyCons)
import           Language.Memento.Typing.Core               (TypingError (..))
import           Language.Memento.Data.Environment.Variance (formatVariance)
import           Text.Megaparsec                            (ParseErrorBundle,
                                                             SourcePos,
                                                             bundleErrors,
                                                             errorOffset,
                                                             parse,
                                                             parseErrorPretty,
                                                             sourceColumn,
                                                             sourceLine)
import           Text.Megaparsec.Pos                        (unPos)

-- | Compute diagnostics for a Memento source file
computeDiagnostics :: Uri -> Text -> IO [Diagnostic]
computeDiagnostics uri content = do
  case parse (parseAST @KProgram) (T.unpack $ getUri uri) content of
    Left errorBundle -> 
      pure $ parseToDiagnostics errorBundle content
    Right parsedProgram ->
      case typeProgramWithTyCons parsedProgram of
        Left typingError ->
          pure [typingErrorToDiagnostic typingError]
        Right (typedProgramWithConstraints, tyConsWithMaybeVariances) -> do
          let solvedVariances = solveVariancesFromEnv tyConsWithMaybeVariances
              varianceErrors = checkVarianceConsistency solvedVariances
          if not (null varianceErrors)
            then pure $ varianceErrorsToDiagnostics varianceErrors
            else case solveTypedAST solvedVariances typedProgramWithConstraints of
                   Left solveError ->
                     pure $ solveErrorToDiagnostics solveError
                   Right _ ->
                     pure []

-- | Convert parse errors to LSP diagnostics
parseToDiagnostics :: ParseErrorBundle Text Void -> Text -> [Diagnostic]
parseToDiagnostics bundle content = 
  case bundleErrors bundle of
    err :| _ -> [parseSingleError err]
    where
      parseSingleError e = Diagnostic
        { _range = offsetToRange (errorOffset e) content :: Range
        , _severity = Just DiagnosticSeverity_Error
        , _code = Nothing
        , _codeDescription = Nothing
        , _source = Just "memento-lsp"
        , _message = T.pack $ parseErrorPretty e
        , _tags = Nothing
        , _relatedInformation = Nothing
        , _data_ = Nothing
        }

-- | Convert solve errors to diagnostics
solveErrorToDiagnostics :: SolveError -> [Diagnostic]
solveErrorToDiagnostics = \case
  ContradictionError msg -> [Diagnostic
    { _range = Range (Position 0 0) (Position 0 0) :: Range  -- TODO: Extract position from error
    , _severity = Just DiagnosticSeverity_Error
    , _code = Nothing
    , _codeDescription = Nothing
    , _source = Just "memento-lsp"
    , _message = T.pack msg
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }]
  UnsolvedVariablesError vars -> [Diagnostic
    { _range = Range (Position 0 0) (Position 0 0) :: Range  -- TODO: Extract position from error
    , _severity = Just DiagnosticSeverity_Error
    , _code = Nothing
    , _codeDescription = Nothing
    , _source = Just "memento-lsp"
    , _message = "Unsolved type variables: " <> T.pack (show vars)
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }]

-- | Convert an offset to a range (placeholder implementation)
offsetToRange :: Int -> Text -> Range
offsetToRange offset content = 
  let (line, col) = offsetToLineCol offset content
  in Range (Position line col) (Position line (col + 1))

-- | Convert offset to line and column (0-indexed)
offsetToLineCol :: Int -> Text -> (UInt, UInt)
offsetToLineCol offset content =
  let textLines = T.lines $ T.take offset content
      lineNum = fromIntegral (length textLines - 1)
      colNum = case textLines of
        [] -> 0
        _  -> fromIntegral $ T.length (last textLines)
  in (max 0 lineNum, colNum)

-- | Convert Megaparsec SourcePos pair to LSP Range
sourcePosRangeToRange :: (SourcePos, SourcePos) -> Range
sourcePosRangeToRange (startPos, endPos) = 
  let startLine = fromIntegral (unPos (sourceLine startPos)) - 1  -- Convert from 1-indexed to 0-indexed
      startCol = fromIntegral (unPos (sourceColumn startPos)) - 1
      endLine = fromIntegral (unPos (sourceLine endPos)) - 1
      endCol = fromIntegral (unPos (sourceColumn endPos)) - 1
  in Range (Position startLine startCol) (Position endLine endCol)

-- | Convert TypingError to LSP Diagnostic
typingErrorToDiagnostic :: TypingError -> Diagnostic
typingErrorToDiagnostic err = Diagnostic
  { _range = case getErrorPosRange err of
      Just posRange -> sourcePosRangeToRange posRange
      Nothing  -> Range (Position 0 0) (Position 0 0)
  , _severity = Just DiagnosticSeverity_Error
  , _code = Nothing
  , _codeDescription = Nothing
  , _source = Just "memento-lsp"
  , _message = T.pack $ formatTypingError err
  , _tags = Nothing
  , _relatedInformation = Nothing
  , _data_ = Nothing
  }
  where
    getErrorPosRange :: TypingError -> Maybe (SourcePos, SourcePos)
    getErrorPosRange = \case
      UndefinedTypeConstructor _ posRange -> posRange
      UndefinedValueConstructor _ posRange -> posRange
      UndefinedVariable _ posRange -> posRange
      ArityMismatch _ _ _ posRange -> posRange
      TypeVariableNotInScope _ posRange -> posRange
    
    formatTypingError :: TypingError -> String
    formatTypingError = \case
      UndefinedTypeConstructor name _ -> 
        "Undefined type constructor: " ++ T.unpack name
      UndefinedValueConstructor name _ -> 
        "Undefined value constructor: " ++ T.unpack name
      UndefinedVariable name _ -> 
        "Undefined variable: " ++ T.unpack name
      ArityMismatch name expected actual _ -> 
        "Arity mismatch for " ++ T.unpack name ++ ": expected " ++ show expected ++ " arguments, but got " ++ show actual
      TypeVariableNotInScope name _ -> 
        "Type variable not in scope: " ++ T.unpack name

-- | Convert variance errors to diagnostics
varianceErrorsToDiagnostics :: [VarianceError] -> [Diagnostic]
varianceErrorsToDiagnostics = map varianceErrorToDiagnostic
  where
    varianceErrorToDiagnostic :: VarianceError -> Diagnostic
    varianceErrorToDiagnostic VarianceError{..} = Diagnostic
      { _range = Range (Position 0 0) (Position 0 0)  -- TODO: Extract position from error
      , _severity = Just DiagnosticSeverity_Error
      , _code = Nothing
      , _codeDescription = Nothing
      , _source = Just "memento-lsp"
      , _message = T.concat
          [ "Variance error in "
          , veTyConsName
          , "."
          , veValConsName
          , " at index "
          , T.pack (show veVarianceIdx)
          , ": \""
          , formatVariance veActualVariance
          , "\" is not a subvariance of \""
          , formatVariance veExpectedVariance
          , "\""
          ]
      , _tags = Nothing
      , _relatedInformation = Nothing
      , _data_ = Nothing
      }