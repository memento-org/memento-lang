{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Language.Memento.Parser.MType (parseMType) where

import Data.Text (Text)
import Language.Memento.Data.AST.MType (MType (TApplication, TBool, TFunction, TInt, TIntersection, TLiteral, TNever, TNumber, TString, TUnion, TUnknown, TVar))
import Language.Memento.Data.AST.Tag (KLiteral, KType, KTypeVariable, KVariable)
import Language.Memento.Data.Functor.Combinator.Higher (Family)
import Language.Memento.Parser.Core (parseAngleBrackets, parseParens, parseReservedWord, parseSymbol)
import Text.Megaparsec (MonadParsec, choice, many, sepBy, try, (<|>), (<?>))

-- | Parse a type with union and intersection support
parseMType ::
  forall f m s. (MonadParsec s Text m) => 
  (forall a. m (MType f a) -> m (f a)) -> -- wrapper function
  Family m f -> 
  m (MType f KType)
parseMType wrap r = parseTypeUnion
  where
    -- Parse union type: A | B | C (lowest precedence)
    parseTypeUnion = do
      first <- parseTypeIntersection
      rest <- many (parseSymbol "|" >> parseTypeIntersection)
      case rest of
        [] -> return first
        _ -> do
          -- Use wrapper to convert each MType to f KType
          wrappedTypes <- traverse (wrap . pure) (first : rest)
          return $ TUnion wrappedTypes

    -- Parse intersection type: A & B & C (higher precedence than union)
    parseTypeIntersection = do
      first <- parseTypeFunction
      rest <- many (parseSymbol "&" >> parseTypeFunction)
      case rest of
        [] -> return first
        _ -> do
          -- Use wrapper to convert each MType to f KType
          wrappedTypes <- traverse (wrap . pure) (first : rest)
          return $ TIntersection wrappedTypes

    -- Parse function type: (arg : type, ...) => returnType
    parseTypeFunction =
      try functionType <|> parseTypeAtom
      where
        functionType = do
          -- Parse argument types inside parentheses
          args <- parseParens $ sepBy argumentPair (parseSymbol ",")
          parseSymbol "=>"
          ret <- r @KType
          return $ TFunction args ret
        
        argumentPair = do
          name <- r @KVariable
          parseSymbol ":"
          typ <- r @KType
          return (name, typ)

    -- Parse atomic types (highest precedence)
    parseTypeAtom =
      choice
        [ parseParens parseTypeUnion <?> "parenthesized type"
        , try parseTypeApplication <?> "type application"
        , parseVarType <?> "type variable"
        , parsePrimitiveType <?> "primitive type"
        , parseLiteralType <?> "literal type"
        , parseUnknownType <?> "unknown type"
        , parseNeverType <?> "never type"
        ]

    -- Parse a variable type
    parseVarType = do
      var <- r @KTypeVariable
      return $ TVar var

    -- Parse a type application like List<T> or Map<K, V>
    parseTypeApplication = do
      baseVar <- r @KTypeVariable
      args <- parseAngleBrackets $ sepBy (r @KType) (parseSymbol ",")
      return $ TApplication baseVar args

    -- Parse a primitive type
    parsePrimitiveType =
      choice
        [ parseReservedWord "number" >> return TNumber
        , parseReservedWord "int" >> return TInt
        , parseReservedWord "bool" >> return TBool
        , parseReservedWord "string" >> return TString
        ] <?> "primitive type"

    -- Parse a literal type
    parseLiteralType = do
      lit <- r @KLiteral
      return $ TLiteral lit

    -- Parse an unknown type
    parseUnknownType = do
      parseReservedWord "unknown"
      return TUnknown

    -- Parse a never type
    parseNeverType = do
      parseReservedWord "never"
      return TNever