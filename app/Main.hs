{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import qualified Data.Text.IO                               as TIO
import           Language.Memento.Backend.JS.Codegen        (formatJSProgram)
import           Language.Memento.Backend.JS.Compile        (compileToJS)
import           Language.Memento.Data.AST.Tag              (KProgram)
import           Language.Memento.Data.Environment.Ty       (TyCons)
import           Language.Memento.Data.Environment.Variance (Variance)
import           Language.Memento.Data.Ty                   (Ty)
import           Language.Memento.Data.TypedAST             (TypedAST)
import           Language.Memento.Parser                    (parseAST)
import           Language.Memento.TypeSolver                (SolveError (..),
                                                             solveTypedAST)
import           Language.Memento.TypeSolver.SolveVariances (solveVariancesFromEnv)
import           Language.Memento.Typing                    (TypingError,
                                                             typeProgramWithTyCons)
import           System.Directory                           (createDirectoryIfMissing)
import           System.Environment                         (getArgs)
import           System.Exit                                (exitFailure,
                                                             exitSuccess)
import           System.FilePath                            (replaceExtension,
                                                             takeFileName,
                                                             (</>))
import           System.IO                                  (hPrint, hPutStrLn,
                                                             stderr)
import           System.Process                             (callProcess)
import           Text.Megaparsec                            (errorBundlePretty,
                                                             parse)

-- | Result of successful compilation pipeline
data CompilationResult = CompilationResult
  { crTypedAST :: TypedAST Ty KProgram
  , crVariances :: TyCons Variance  
  } deriving (Show)

-- | Errors that can occur during compilation pipeline
data CompilationError
  = ParseError String
  | TypingError TypingError  
  | SolvingError SolveError
  deriving (Show)

-- | Core compilation pipeline: parse -> type -> solve
-- This function extracts the common logic shared by all three commands
compileMemento :: FilePath -> IO (Either CompilationError CompilationResult)
compileMemento filePath = do
  -- Read the file
  contents <- TIO.readFile filePath
  
  -- Parse the file
  case parse (parseAST @KProgram) filePath contents of
    Left errorBundle -> 
      return $ Left $ ParseError $ errorBundlePretty errorBundle
    Right parsedProgram -> do
      -- Type check the AST
      case typeProgramWithTyCons parsedProgram of
        Left typingError ->
          return $ Left $ TypingError typingError
        Right (typedProgramWithConstraints, tyConsWithMaybeVariances) -> do
          -- Solve variances
          let solvedVariances = solveVariancesFromEnv tyConsWithMaybeVariances
          
          -- Solve type constraints
          case solveTypedAST solvedVariances typedProgramWithConstraints of
            Left solveError ->
              return $ Left $ SolvingError solveError
            Right fullyTypedProgram ->
              return $ Right $ CompilationResult fullyTypedProgram solvedVariances

-- | Generate JavaScript code from compilation result and write to file
-- This function extracts the common logic shared by compileCommand and runCommand
generateJavaScript :: CompilationResult -> FilePath -> IO FilePath
generateJavaScript (CompilationResult fullyTypedProgram _) sourceFilePath = do
  -- Compile to JS IR
  let jsIR = compileToJS fullyTypedProgram
  
  -- Generate JavaScript code  
  let jsCode = formatJSProgram jsIR
  let outputFile = "dist" </> replaceExtension (takeFileName sourceFilePath) ".js"
  
  -- Create dist directory if it doesn't exist
  createDirectoryIfMissing True "dist"
  
  -- Write to output file
  TIO.writeFile outputFile jsCode
  
  return outputFile

-- | Handle compilation errors with appropriate error messages and exit
handleCompilationError :: CompilationError -> IO a
handleCompilationError = \case
  ParseError err -> do
    hPutStrLn stderr "Parse error:"
    hPutStrLn stderr err
    exitFailure
  TypingError typingError -> do
    hPutStrLn stderr "Type error:"
    hPrint stderr typingError
    exitFailure
  SolvingError (ContradictionError err) -> do
    hPutStrLn stderr "Type constraint solving error:"
    hPutStrLn stderr err
    exitFailure
  SolvingError (UnsolvedVariablesError vars) -> do
    hPutStrLn stderr "Unsolved type variables:"
    hPrint stderr vars
    exitFailure

-- | Main entry point for the Memento compiler
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      hPutStrLn stderr "Usage: memento-compiler <command> [options]"
      hPutStrLn stderr ""
      hPutStrLn stderr "Commands:"
      hPutStrLn stderr "  check <file>    Parse and type-check a Memento source file"
      hPutStrLn stderr "  compile <file>  Compile a Memento source file to JavaScript"
      hPutStrLn stderr "  run <file>      Compile and run a Memento source file"
      hPutStrLn stderr ""
      hPutStrLn stderr "Examples:"
      hPutStrLn stderr "  memento-compiler check example.mmt"
      hPutStrLn stderr "  memento-compiler compile example.mmt"
      hPutStrLn stderr "  memento-compiler run example.mmt"
      exitFailure
    ["check", filePath] -> checkCommand filePath
    ["compile", filePath] -> compileCommand filePath
    ["run", filePath] -> runCommand filePath
    _ -> do
      hPutStrLn stderr "Invalid command. Run 'memento-compiler' without arguments for usage."
      exitFailure

-- | Parse and type-check a Memento source file
checkCommand :: FilePath -> IO ()
checkCommand filePath = do
  putStrLn $ "Type checking: " ++ filePath
  putStrLn $ replicate 60 '-'

  result <- compileMemento filePath
  case result of
    Left err -> handleCompilationError err
    Right _ -> do
      putStrLn "Parse successful!"
      putStrLn $ replicate 60 '-'
      putStrLn "✓ The file was successfully type checked."
      exitSuccess

-- | Compile a Memento source file to JavaScript
compileCommand :: FilePath -> IO ()
compileCommand filePath = do
  putStrLn $ "Compiling: " ++ filePath
  putStrLn $ replicate 60 '-'

  result <- compileMemento filePath
  case result of
    Left err -> handleCompilationError err
    Right compilationResult -> do
      putStrLn "Parse successful!"
      putStrLn "Type constraint solving successful!"
      putStrLn "Compilation to JS IR successful!"
      
      outputFile <- generateJavaScript compilationResult filePath
      
      putStrLn $ "✓ Successfully compiled to: " ++ outputFile
      putStrLn ""
      exitSuccess

-- | Compile and run a Memento source file
runCommand :: FilePath -> IO ()
runCommand filePath = do
  putStrLn $ "Compiling and running: " ++ filePath
  putStrLn $ replicate 60 '-'

  result <- compileMemento filePath
  case result of
    Left err -> handleCompilationError err
    Right compilationResult -> do
      putStrLn "Parse successful!"
      putStrLn "Type constraint solving successful!"
      putStrLn "Compilation to JS IR successful!"
      
      outputFile <- generateJavaScript compilationResult filePath
      
      putStrLn $ "✓ Successfully compiled to: " ++ outputFile
      putStrLn ""
      putStrLn "Running JavaScript:"
      putStrLn $ replicate 60 '-'

      -- Run the JavaScript with Node.js
      callProcess "node" [outputFile]
      putStrLn ""
      putStrLn "✓ Execution completed"
      exitSuccess
