{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import qualified Data.Text.IO                  as TIO
import           Language.Memento.Data.AST.Tag (KProgram)
import           Language.Memento.Parser       (parseAST)
import           System.Environment            (getArgs)
import           System.Exit                   (exitFailure, exitSuccess)
import           System.IO                     (hPutStrLn, stderr)
import           Text.Megaparsec               (errorBundlePretty, parse)

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
      hPutStrLn stderr "  check <file>    Parse and type-check a Memento source file (not implemented)"
      hPutStrLn stderr "  compile <file>  Compile a Memento source file (not implemented)"
      hPutStrLn stderr ""
      hPutStrLn stderr "Examples:"
      hPutStrLn stderr "  memento-compiler parse example.mmt"
      exitFailure
    ["parse", filePath] -> do
      parseCommand filePath
    ["check", _] -> do
      hPutStrLn stderr "Type checking is not yet implemented."
      exitFailure
    ["compile", _] -> do
      hPutStrLn stderr "Code generation is not yet implemented."
      exitFailure
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
