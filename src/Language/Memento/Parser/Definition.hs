{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Language.Memento.Parser.Definition (parseDefinition) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import GHC.List (List)
import Language.Memento.Data.AST.Definition (ConstructorDef (ConstructorDef, ConstructorFnDef), Definition (DataDef, FnDef, TypeDef, ValDef))
import Language.Memento.Data.AST.Tag (KBlock, KDefinition, KExpr, KType, KTypeVariable, KVariable)
import Language.Memento.Data.Functor.Combinator.Higher (Family)
import Language.Memento.Parser.Core (parseAngleBrackets, parseBrackets, parseParens, parseReservedWord, parseSymbol)
import Text.Megaparsec (MonadParsec, choice, sepBy, try, (<?>))

parseDefinition :: forall f m s. (MonadParsec s Text m) => Family m f -> m (Definition f KDefinition)
parseDefinition r =
  choice
    [ parseValDefinition r <?> "val definition"
    , parseFnDefinition r <?> "fn definition"
    , parseDataDefinition r <?> "data definition"
    , parseTypeDef r <?> "type definition"
    ]

{- | Parse individual constructor: ConstructorName<T, U> : (args...) => ReturnType
| or new syntax: ConstructorName<T, U>(args...) : ReturnType
-}
parseConstructorDef ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (ConstructorDef f)
parseConstructorDef r = do
  choice
    [ parseFunctionSyntax <?> "function constructor definition"
    , parseNormalSyntax <?> "value constructor definition"
    ]
 where
  -- Normal syntax: val Nil<T> : List<T>
  parseNormalSyntax = do
    parseReservedWord "val"
    constructorName <- r @KVariable
    ctorTypeParams <- parseTypeParameters r
    parseSymbol ":"
    constructorType <- r @KType
    return $ ConstructorDef constructorName ctorTypeParams constructorType

  -- Function syntax: fn Cons<T>(x : T, xs : List<T>) : List<T>
  parseFunctionSyntax = do
    parseReservedWord "fn"
    constructorName <- r @KVariable
    ctorTypeParams <- parseTypeParameters r
    -- Parse parameter list
    params <-
      parseParens $
        sepBy
          ( do
              paramName <- r @KVariable
              parseSymbol ":"
              paramType <- r @KType
              return (paramName, paramType)
          )
          (parseSymbol ",")
    -- Parse return type
    parseSymbol ":"
    returnType <- r @KType

    return $ ConstructorFnDef constructorName ctorTypeParams params returnType

-- | data Maybe [fn Some<T>(x : T) : Maybe<T>, val None<T> : Maybe<T>];
parseDataDefinition ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (Definition f KDefinition)
parseDataDefinition r = do
  parseReservedWord "data"
  dataName <- r @KVariable
  constructors <- parseBrackets $ sepBy (parseConstructorDef r) (parseSymbol ",")
  parseSymbol ";"
  return $ DataDef dataName constructors

parseValDefinition ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (Definition f KDefinition)
parseValDefinition r = do
  parseReservedWord "val"
  name <- r @KVariable
  typeParams <- parseTypeParameters r
  parseSymbol ":"
  typ <- r @KType
  parseSymbol "="
  body <- r @KExpr
  return $ ValDef name typeParams typ body

parseFnDefinition ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (Definition f KDefinition)
parseFnDefinition r = do
  parseReservedWord "fn"
  name <- r @KVariable
  typeParams <- parseTypeParameters r
  -- Parse parameter list
  params <-
    parseParens $
      sepBy
        ( do
            paramName <- r @KVariable
            parseSymbol ":"
            paramType <- r @KType
            return (paramName, paramType)
        )
        (parseSymbol ",")
  parseSymbol ":"
  typ <- r @KType
  body <- r @KBlock
  return $ FnDef name typeParams params typ body

parseTypeDef ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (Definition f KDefinition)
parseTypeDef r = do
  parseReservedWord "type"
  name <- r @KVariable
  typeParams <- parseTypeParameters r
  parseSymbol "="
  typ <- r @KType
  _ <- parseSymbol ";"
  return $ TypeDef name typeParams typ

-- | Parse optional type parameter list like <T, U, V>
parseTypeParameters ::
  forall f m s. (MonadParsec s Text m) => Family m f -> m (List (f KTypeVariable))
parseTypeParameters r = do
  try (parseAngleBrackets $ sepBy (r @KTypeVariable) (parseSymbol ","))
    <|> return [] -- Empty list if no type parameters
