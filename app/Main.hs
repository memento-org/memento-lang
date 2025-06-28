{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import qualified Data.Text                                as T
import qualified Data.Text.IO                            as TIO
import           Language.Memento.Backend.JS.Codegen     (formatJSProgram)
import           Language.Memento.Backend.JS.Compile     (compileToJS)
import           Language.Memento.Data.AST.Tag           (KProgram)
import           Language.Memento.Parser                 (parseAST)
import           Language.Memento.Typing                 (TypingError, typeProgram)
import           System.Directory                        (createDirectoryIfMissing)
import           System.Environment                      (getArgs)
import           System.Exit                             (exitFailure, exitSuccess)
import           System.FilePath                         (replaceExtension, takeFileName, (</>))
import           System.IO                               (hPutStrLn, stderr)
import           System.Process                          (callProcess)
import           Text.Megaparsec                         (errorBundlePretty, parse)

-- | Main entry point for the Memento compiler
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      hPutStrLn stderr "Usage: memento-compiler <command> [options]"
      hPutStrLn stderr ""
      hPutStrLn stderr "Commands:"
      hPutStrLn stderr "  parse <file>    Parse a Memento source file and show AST"
      hPutStrLn stderr "  check <file>    Parse and type-check a Memento source file"
      hPutStrLn stderr "  compile <file>  Compile a Memento source file to JavaScript"
      hPutStrLn stderr "  run <file>      Compile and run a Memento source file"
      hPutStrLn stderr ""
      hPutStrLn stderr "Examples:"
      hPutStrLn stderr "  memento-compiler parse example.mmt"
      hPutStrLn stderr "  memento-compiler check example.mmt"
      hPutStrLn stderr "  memento-compiler compile example.mmt"
      hPutStrLn stderr "  memento-compiler run example.mmt"
      exitFailure
    ["parse", filePath] -> do
      parseCommand filePath
    ["check", filePath] -> do
      checkCommand filePath
    ["compile", filePath] -> do
      compileCommand filePath
    ["run", filePath] -> do
      runCommand filePath
    _ -> do
      hPutStrLn stderr "Invalid command. Run 'memento-compiler' without arguments for usage."
      exitFailure

-- | Parse a Memento source file and display the result
parseCommand :: FilePath -> IO ()
parseCommand filePath = do
  putStrLn $ "Parsing: " ++ filePath
  putStrLn $ replicate 60 '-'

  -- Read the file
  contents <- TIO.readFile filePath

  -- Parse the file
  case parse (parseAST @KProgram) filePath contents of
    Left errorBundle -> do
      hPutStrLn stderr "Parse error:"
      hPutStrLn stderr $ errorBundlePretty errorBundle
      exitFailure
    Right ast -> do
      putStrLn "Parse successful!"
      putStrLn ""
      putStrLn "AST:"
      putStrLn $ replicate 60 '-'
      print ast
      putStrLn ""
      putStrLn "✓ The file was successfully parsed."
      exitSuccess

-- | Parse and type-check a Memento source file
checkCommand :: FilePath -> IO ()
checkCommand filePath = do
  putStrLn $ "Type checking: " ++ filePath
  putStrLn $ replicate 60 '-'

  -- Read the file
  contents <- TIO.readFile filePath

  -- Parse the file
  case parse (parseAST @KProgram) filePath contents of
    Left errorBundle -> do
      hPutStrLn stderr "Parse error:"
      hPutStrLn stderr $ errorBundlePretty errorBundle
      exitFailure
    Right ast -> do
      putStrLn "Parse successful!"

      -- Type check the AST
      case typeProgram ast of
        Left typingError -> do
          hPutStrLn stderr "Type error:"
          hPutStrLn stderr $ show typingError
          exitFailure
        Right typedAst -> do
          putStrLn "Type checking successful!"
          putStrLn ""
          putStrLn "Typed AST:"
          putStrLn $ replicate 60 '-'
          print typedAst
          putStrLn ""
          putStrLn "✓ The file was successfully type checked."
          exitSuccess

-- | Compile a Memento source file to JavaScript
compileCommand :: FilePath -> IO ()
compileCommand filePath = do
  putStrLn $ "Compiling: " ++ filePath
  putStrLn $ replicate 60 '-'

  -- Read the file
  contents <- TIO.readFile filePath

  -- Parse the file
  case parse (parseAST @KProgram) filePath contents of
    Left errorBundle -> do
      hPutStrLn stderr "Parse error:"
      hPutStrLn stderr $ errorBundlePretty errorBundle
      exitFailure
    Right ast -> do
      putStrLn "Parse successful!"

      -- Type check the AST
      case typeProgram ast of
        Left typingError -> do
          hPutStrLn stderr "Type error:"
          hPutStrLn stderr $ show typingError
          exitFailure
        Right typedAst -> do
          putStrLn "Type checking successful!"

          -- Compile to JS IR
          let jsIR = compileToJS typedAst
          putStrLn "Compilation to JS IR successful!"

          -- Generate JavaScript code
          let jsCode = formatJSProgram jsIR
          let outputFile = "dist" </> replaceExtension (takeFileName filePath) ".js"

          -- Create dist directory if it doesn't exist
          createDirectoryIfMissing True "dist"

          -- Write to output file
          TIO.writeFile outputFile jsCode

          putStrLn $ "✓ Successfully compiled to: " ++ outputFile
          putStrLn ""
          exitSuccess

-- | Compile and run a Memento source file
runCommand :: FilePath -> IO ()
runCommand filePath = do
  putStrLn $ "Compiling and running: " ++ filePath
  putStrLn $ replicate 60 '-'

  -- Read the file
  contents <- TIO.readFile filePath

  -- Parse the file
  case parse (parseAST @KProgram) filePath contents of
    Left errorBundle -> do
      hPutStrLn stderr "Parse error:"
      hPutStrLn stderr $ errorBundlePretty errorBundle
      exitFailure
    Right ast -> do
      putStrLn "Parse successful!"

      -- Type check the AST
      case typeProgram ast of
        Left typingError -> do
          hPutStrLn stderr "Type error:"
          hPutStrLn stderr $ show typingError
          exitFailure
        Right typedAst -> do
          putStrLn "Type checking successful!"

          -- Compile to JS IR
          let jsIR = compileToJS typedAst
          putStrLn "Compilation to JS IR successful!"

          -- Generate JavaScript code
          let jsCode = formatJSProgram jsIR
          let outputFile = "dist" </> replaceExtension (takeFileName filePath) ".js"

          -- Create dist directory if it doesn't exist
          createDirectoryIfMissing True "dist"

          -- Write to output file
          TIO.writeFile outputFile jsCode

          putStrLn $ "✓ Successfully compiled to: " ++ outputFile
          putStrLn ""
          putStrLn "Running JavaScript:"
          putStrLn $ replicate 60 '-'
          
          -- Run the JavaScript with Node.js
          callProcess "node" [outputFile]
          putStrLn ""
          putStrLn "✓ Execution completed"
          exitSuccess
