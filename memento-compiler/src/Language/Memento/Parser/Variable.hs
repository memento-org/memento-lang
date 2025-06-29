{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE RankNTypes       #-}


module Language.Memento.Parser.Variable (parseVariable, parseTypeVariable) where

import           Data.Text                                       (Text)
import           Language.Memento.Data.AST.Tag                   (KTypeVariable,
                                                                  KVariable)
import           Language.Memento.Data.AST.Variable              (TypeVariable (TypeVar),
                                                                  Variable (Var))
import           Language.Memento.Data.Functor.Combinator.Higher (Family,
                                                                  Wrapper)
import           Language.Memento.Parser.Core                    (parseIdentifier)
import           Text.Megaparsec                                 (MonadParsec)

-- | Parse a variable
parseVariable ::
  forall f m s. (MonadParsec s Text m, MonadFail m) =>
  Wrapper m Variable f ->
  Family m f ->
  m (f KVariable)
parseVariable wrap _ = wrap $ Var <$> parseIdentifier

-- | Parse a type variable
parseTypeVariable ::
  forall f m s. (MonadParsec s Text m, MonadFail m) =>
  Wrapper m TypeVariable f ->
  Family m f ->
  m (f KTypeVariable)
parseTypeVariable wrap _ = wrap $ TypeVar <$> parseIdentifier
