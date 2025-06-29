{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

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
import           Language.Memento.TypeSolver.SolveVariances (solveVariancesFromEnv)
import           Language.Memento.Typing                    (typeProgramWithTyCons)
import           Text.Megaparsec                            (ParseError,
                                                             ParseErrorBundle,
                                                             SourcePos (..),
                                                             bundleErrors,
                                                             errorOffset,
                                                             parseErrorPretty,
                                                             unPos)
import           Text.Megaparsec.Error                      (ParseError (..))
import           Text.Megaparsec.Pos                        (mkPos)

-- | Compute diagnostics for a Memento source file
computeDiagnostics :: Uri -> Text -> IO [Diagnostic]
computeDiagnostics uri content = do
  case parseAST @KProgram (T.unpack $ getUri uri) content of
    Left errorBundle -> 
      pure $ parseToDiagnostics errorBundle
    Right parsedProgram ->
      case typeProgramWithTyCons parsedProgram of
        Left typingError ->
          -- TODO: Convert typing errors to diagnostics with proper positions
          pure [Diagnostic
            { _range = Range (Position 0 0) (Position 0 0)
            , _severity = Just DiagnosticSeverity_Error
            , _code = Nothing
            , _codeDescription = Nothing
            , _source = Just "memento-lsp"
            , _message = T.pack $ show typingError
            , _tags = Nothing
            , _relatedInformation = Nothing
            , _data_ = Nothing
            }]
        Right (typedProgramWithConstraints, tyConsWithMaybeVariances) -> do
          let solvedVariances = solveVariancesFromEnv tyConsWithMaybeVariances
          case solveTypedAST solvedVariances typedProgramWithConstraints of
            Left solveError ->
              pure $ solveErrorToDiagnostics solveError
            Right _ ->
              pure []

-- | Convert parse errors to LSP diagnostics
parseToDiagnostics :: ParseErrorBundle Text Void -> [Diagnostic]
parseToDiagnostics bundle = 
  case bundleErrors bundle of
    err :| _ -> [parseSingleError err]
    where
      parseSingleError e = Diagnostic
        { _range = offsetToRange (errorOffset e) content
        , _severity = Just DiagnosticSeverity_Error
        , _code = Nothing
        , _codeDescription = Nothing
        , _source = Just "memento-lsp"
        , _message = T.pack $ parseErrorPretty e
        , _tags = Nothing
        , _relatedInformation = Nothing
        , _data_ = Nothing
        }
      content = ""  -- TODO: Get actual content from bundle

-- | Convert solve errors to diagnostics
solveErrorToDiagnostics :: SolveError -> [Diagnostic]
solveErrorToDiagnostics = \case
  ContradictionError msg -> [Diagnostic
    { _range = Range (Position 0 0) (Position 0 0)  -- TODO: Extract position from error
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
    { _range = Range (Position 0 0) (Position 0 0)  -- TODO: Extract position from error
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
  let lines = T.lines $ T.take offset content
      lineNum = fromIntegral (length lines - 1)
      colNum = case lines of
        [] -> 0
        _  -> fromIntegral $ T.length (last lines)
  in (max 0 lineNum, colNum)