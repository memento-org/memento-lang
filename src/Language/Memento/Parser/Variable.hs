{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Language.Memento.Parser.Variable (parseVariable, parseTypeVariable) where

import Data.Text (Text)
import Language.Memento.Data.AST.Tag (KTypeVariable, KVariable)
import Language.Memento.Data.AST.Variable (TypeVariable (TypeVar), Variable (Var))
import Language.Memento.Data.Functor.Combinator.Higher (Family)
import Language.Memento.Parser.Core (parseIdentifier)
import Text.Megaparsec (MonadParsec)

-- | Parse a variable
parseVariable ::
  forall f m s. (MonadParsec s Text m, MonadFail m) => Family m f -> m (Variable f KVariable)
parseVariable _ = do
  name <- parseIdentifier
  return $ Var name

-- | Parse a type variable
parseTypeVariable ::
  forall f m s. (MonadParsec s Text m, MonadFail m) => Family m f -> m (TypeVariable f KTypeVariable)
parseTypeVariable _ = do
  name <- parseIdentifier
  return $ TypeVar name