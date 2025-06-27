{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Memento.Parser.Expr (parseExpr, parseLet, parseBlock) where

import           Control.Applicative                             ((<|>))
import           Control.Monad.Combinators.Expr                  (Operator (InfixL),
                                                                  makeExprParser)
import           Data.Text                                       (Text)
import           Language.Memento.Data.AST.Expr                  (BinOp (..),
                                                                  Block (Block),
                                                                  Expr (EApply, EBinOp, EBlock, EIf, ELambda, ELiteral, EMatch, EVar),
                                                                  Let (Let))
import           Language.Memento.Data.AST.Tag                   (KBlock, KExpr,
                                                                  KLet,
                                                                  KLiteral,
                                                                  KPattern,
                                                                  KType,
                                                                  KVariable)
import           Language.Memento.Data.Functor.Combinator.Higher (Family,
                                                                  Wapper)
import           Language.Memento.Parser.Core                    (parseBraces,
                                                                  parseParens,
                                                                  parseReservedWord,
                                                                  parseSymbol)
import           Text.Megaparsec                                 (MonadParsec,
                                                                  choice, many,
                                                                  optional,
                                                                  sepEndBy, try,
                                                                  (<?>))

makeBinOp ::
  (MonadParsec s Text m) =>
  ((f KExpr -> f KExpr -> Expr f KExpr) -> f KExpr -> f KExpr -> f KExpr) -> -- Propagate
  Text ->
  BinOp ->
  m (f KExpr -> f KExpr -> f KExpr)
makeBinOp propagate opText binOp = do
                 parseSymbol opText
                 pure $ \left right -> propagate (EBinOp binOp) left right

-- | Operator precedence table using propagate function and makeExprParser
operatorTable ::
  forall f m s.
  (MonadParsec s Text m) =>
  ((f KExpr -> f KExpr -> Expr f KExpr) -> f KExpr -> f KExpr -> f KExpr) -> -- propagate function
  [[Operator m (f KExpr)]]
operatorTable propagate  =
  [ -- Highest precedence: multiplicative operators
    [ InfixL $ makeBinOp propagate "*" Mul
    , InfixL $ makeBinOp propagate "/" Div
    ]
  , -- Lower precedence: additive operators
    [ InfixL $ makeBinOp propagate "+" Add
    , InfixL $ makeBinOp propagate "-" Sub
    ]
  , -- Lowest precedence: comparison operators
    [ InfixL $ makeBinOp propagate "==" Eq
    , InfixL $ makeBinOp propagate "<" Lt
    , InfixL $ makeBinOp propagate ">" Gt
    ]
  ]

-- | Parse a basic term (without function application)
parseTermBase ::
  forall f m s.
  (MonadParsec s Text m) =>
  Wapper m Expr f ->
  ((f KExpr -> f KExpr -> Expr f KExpr) -> f KExpr -> f KExpr -> f KExpr) -> -- propagate function
  Family m f ->
  m (f KExpr)
parseTermBase wrap propagate r =
  choice
    [ parseBlockExpr <?> "block expression"
    , parseLiteral <?> "literal"
    , parseIfExpr <?> "if expression"
    , parseParenthesized <?> "parenthesized expression"
    , parseVariable <?> "variable"
    ]
  where
    parseVariable = wrap $ EVar <$> r @KVariable

    parseLiteral = wrap $ ELiteral <$> r @KLiteral

    parseIfExpr = wrap $ do
      parseReservedWord "if"
      cond <- parseParens (r @KExpr)
      thenExpr <- parseBlockExpr
      parseReservedWord "else"
      EIf cond thenExpr <$> parseBlockExpr

    parseParenthesized = parseParens (parseExpr wrap propagate r)

    parseBlockExpr = wrap $ do
      block <- r @KBlock
      return $ EBlock block

-- | Parse a term with function application
parseTerm ::
  forall f m s.
  (MonadParsec s Text m) =>
  Wapper m Expr f ->
  ((f KExpr -> f KExpr -> Expr f KExpr) -> f KExpr -> f KExpr -> f KExpr) -> -- propagate function
  Family m f ->
  m (f KExpr)
parseTerm wrap propagate r = do
  -- Parse the base term first
  term <- parseTermBase wrap propagate r

  -- Then check for function application (zero or more sets of parentheses)
  parseAppChain term
  where
    -- Parse a chain of function applications
    parseAppChain :: f KExpr -> m (f KExpr)
    parseAppChain term =
      -- Try to parse parentheses for function application
      (do
        args <- parseParens $ sepEndBy (r @KExpr) (parseSymbol ",")
        app <- wrap $ return $ EApply term args
        parseAppChain app
      ) <|> return term -- If no parentheses, just return the term

-- | Parse lambda expression
parseLambdaExpr ::
  forall f m s.
  (MonadParsec s Text m) =>
  Wapper m Expr f ->
  Family m f ->
  m (f KExpr)
parseLambdaExpr wrap r = wrap $ do
  parseReservedWord "fn"
  args <- parseParens $ sepEndBy
    (do
      patt <- r @KPattern
      mtype <- optional $ do
        parseSymbol ":"
        r @KType
      return (patt, mtype)
    )
    (parseSymbol ",")
  parseSymbol "->"
  body <- r @KExpr
  return $ ELambda args body

-- | Parse match/switch expression
parseMatchExpr ::
  forall f m s.
  (MonadParsec s Text m) =>
  Wapper m Expr f ->
  Family m f ->
  m (f KExpr)
parseMatchExpr wrap r = wrap $ do
  parseReservedWord "switch"
  args <- parseParens $ sepEndBy (r @KExpr) (parseSymbol ",")
  clauses <- parseBraces $ sepEndBy
    (do
      parseReservedWord "case"
      patterns <- parseParens $ sepEndBy
        (do
          patt <- r @KPattern
          mtype <- optional $ do
            parseSymbol ":"
            r @KType
          return (patt, mtype)
        )
        (parseSymbol ",")
      parseSymbol "->"
      body <- r @KExpr
      return (patterns, body)
    )
    (parseSymbol ",")
  return $ EMatch args clauses

-- | Parse binary operators and terms
parseTermAndBinOp ::
  forall f m s.
  (MonadParsec s Text m) =>
  Wapper m Expr f ->
  ((f KExpr -> f KExpr -> Expr f KExpr) -> f KExpr -> f KExpr -> f KExpr) -> -- propagate function
  Family m f ->
  m (f KExpr)
parseTermAndBinOp wrap propagate r =
  makeExprParser (parseTerm wrap propagate r) (operatorTable propagate)

-- | Parse an expression with all types
parseExpr ::
  forall f m s.
  (MonadParsec s Text m) =>
  Wapper m Expr f ->
  (( f KExpr -> f KExpr -> Expr f KExpr) -> f KExpr -> f KExpr -> f KExpr) -> -- propagate function
  Family m f ->
  m (f KExpr)
parseExpr wrap propagate r =
  choice
    [ parseLambdaExpr wrap r <?> "lambda expression"
    , parseMatchExpr wrap r <?> "match expression"
    , parseTermAndBinOp wrap propagate r <?> "term and binary operator expression"
    ]

-- | Parse a let binding
parseLet ::
  forall f m s. (MonadParsec s Text m) =>
  Wapper m Let f ->
  Family m f ->
  m (f KLet)
parseLet wrap r = wrap $ do
  parseReservedWord "let"
  pattern <- r @KPattern
  mtype <- try (Just <$> (parseSymbol ":" >> r @KType)) <|> return Nothing
  parseSymbol "="
  expr <- r @KExpr
  parseSymbol ";"
  return $ Let pattern mtype expr

parseBlock ::
  forall f m s. (MonadParsec s Text m) =>
  Wapper m Block f ->
  Family m f ->
  m (f KBlock)
parseBlock wrap r = wrap $ do
  parseBraces $ do
    lets <- many (r @KLet)
    expr <- r @KExpr
    return $ Block lets expr

