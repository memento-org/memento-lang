{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Memento.Parser.Pattern (parsePattern) where

import           Data.Text                                       (Text)
import           Language.Memento.Data.AST.Pattern               (Pattern (PCons, PLiteral, PVar, PWildcard))
import           Language.Memento.Data.AST.Tag                   (KLiteral,
                                                                  KPattern,
                                                                  KVariable)
import           Language.Memento.Data.Functor.Combinator.Higher (Family,
                                                                  Wrapper)
import           Language.Memento.Parser.Core                    (parseParens,
                                                                  parseSymbol)
import           Text.Megaparsec                                 (MonadParsec,
                                                                  choice,
                                                                  sepEndBy, try,
                                                                  (<?>))

-- | Parse a pattern
parsePattern ::
  forall f m s. (MonadParsec s Text m) =>
  Wrapper m Pattern f ->
  Family m f ->
  m (f KPattern)
parsePattern wrap r = wrap $
  choice
    [ parseWildcardPattern r <?> "wildcard pattern"
    , parseLiteralPattern r <?> "literal pattern"
    , parseConsPattern r <?> "constructor pattern"
    , parseVarPattern r <?> "variable pattern"
    ]

-- | Parse a variable pattern
parseVarPattern ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (Pattern f KPattern)
parseVarPattern r = do
  var <- r @KVariable
  return $ PVar var

-- | Parse a wildcard pattern
parseWildcardPattern ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (Pattern f KPattern)
parseWildcardPattern _ = do
  _ <- parseSymbol "_"
  return PWildcard

-- | Parse a literal pattern
parseLiteralPattern ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (Pattern f KPattern)
parseLiteralPattern r = do
  lit <- r @KLiteral
  return $ PLiteral lit

-- | Parse a constructor pattern
parseConsPattern ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (Pattern f KPattern)
parseConsPattern r = try $ do
  constructor <- r @KVariable
  args <- parseParens $ sepEndBy (r @KPattern) (parseSymbol ",")
  return $ PCons constructor args
