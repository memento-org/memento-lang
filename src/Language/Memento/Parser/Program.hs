{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Language.Memento.Parser.Program (parseProgram) where

import           Data.Text                                       (Text)
import           Language.Memento.Data.AST.Program               (Program (Program))
import           Language.Memento.Data.AST.Tag                   (KDefinition,
                                                                  KProgram)
import           Language.Memento.Data.Functor.Combinator.Higher (Family,
                                                                  Wrapper)
import           Language.Memento.Parser.Core                    (sc)
import           Text.Megaparsec                                 (MonadParsec,
                                                                  eof, many)

-- | Parse a program, which consists of a series of definitions
parseProgram ::
  forall f m s. (MonadParsec s Text m) =>
  Wrapper m Program f ->
  Family m f ->
  m (f KProgram)
parseProgram wrap r = wrap $ do
  -- Skip any leading whitespace
  sc

  -- Parse all definitions
  defs <- many (r @KDefinition)

  -- Make sure we've consumed the entire input
  eof

  -- Return the program with its definitions
  return $ Program defs
