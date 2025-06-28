{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.Parser.Literal (parseLiteral) where

import           Control.Applicative                             (Alternative ((<|>)))
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import           Language.Memento.Data.AST.Literal               (Literal (BoolLiteral, IntLiteral, NumberLiteral, StringLiteral))
import           Language.Memento.Data.AST.Tag                   (KLiteral)
import           Language.Memento.Data.Functor.Combinator.Higher (Family,
                                                                  Wrapper)
import           Language.Memento.Parser.Core                    (parseLexeme,
                                                                  parseReservedWord)
import           Text.Megaparsec                                 (MonadParsec (try),
                                                                  choice,
                                                                  manyTill,
                                                                  (<?>))
import           Text.Megaparsec.Char                            (char)
import           Text.Megaparsec.Char.Lexer                      as L (charLiteral,
                                                                       decimal,
                                                                       float)

parseLiteral ::
  forall f m s.
  (MonadParsec s Text m) =>
  Wrapper m Literal f ->
  Family m f -> -- Recursive
  m (f KLiteral)
parseLiteral wrap r = wrap $
  choice
    [ try (parseNumberLiteral r) <?> "number literal"
    , try (parseBoolLiteral r) <?> "boolean literal"
    , try (parseIntLiteral r) <?> "integer literal"
    , try (parseStringLiteral r) <?> "string literal"
    ]

parseNumberLiteral :: forall f m s. (MonadParsec s Text m) => Family m f -> m (Literal f KLiteral)
parseNumberLiteral _ = do
  num <- parseLexeme L.float
  return $ NumberLiteral num

parseBoolLiteral :: forall f m s. (MonadParsec s Text m) => Family m f -> m (Literal f KLiteral)
parseBoolLiteral _ = do
  bool <- parseLexeme $ True <$ parseReservedWord "true" <|> False <$ parseReservedWord "false"
  return $ BoolLiteral bool

parseIntLiteral :: forall f m s. (MonadParsec s Text m) => Family m f -> m (Literal f KLiteral)
parseIntLiteral _ = do
  int <- parseLexeme L.decimal
  return $ IntLiteral int

parseStringLiteral :: forall f m s. (MonadParsec s Text m) => Family m f -> m (Literal f KLiteral)
parseStringLiteral _ = do
  str <- parseLexeme (char '"' >> manyTill L.charLiteral (char '"'))
  return $ StringLiteral $ T.pack str
