{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Memento.Parser (parseAST) where

import           Data.Text                                       (Text)
import           Data.Typeable                                   (Typeable)
import           GHC.Base                                        (Void, (<|>))
import           Language.Memento.Data.AST                       (AST, Syntax)
import           Language.Memento.Data.AST.Metadata              (Metadata,
                                                                  propagateMetadata)
import           Language.Memento.Data.Functor.Combinator.Higher (Wapper,
                                                                  quantify, tie)
import           Language.Memento.Data.Functor.Coproduct.Higher  (HInjective (hInject))
import           Language.Memento.Data.Functor.FixedPoint.Higher (HFix (HFix),
                                                                  extractHFix)
import           Language.Memento.Data.Functor.Product.Higher    (HConstructive (hConstruct),
                                                                  HExtractive,
                                                                  HProduct,
                                                                  hSingleton)
import           Language.Memento.Parser.Definition              (parseDefinition)
import           Language.Memento.Parser.Expr                    (parseBlock,
                                                                  parseExpr,
                                                                  parseLet)
import           Language.Memento.Parser.Literal                 (parseLiteral)
import           Language.Memento.Parser.Metadata                (parseMetadata)
import           Language.Memento.Parser.MType                   (parseMType)
import           Language.Memento.Parser.Pattern                 (parsePattern)
import           Language.Memento.Parser.Program                 (parseProgram)
import           Language.Memento.Parser.Variable                (parseTypeVariable,
                                                                  parseVariable)
import           Text.Megaparsec                                 (MonadParsec,
                                                                  Parsec)

{-
x ::
  (MonadParsec s Text m) =>
  (forall x. m (AST x)) -> -- Recursive
  m (AST KLiteral)
x r =
  HFix
    <$> parseMetadata (hSingleton . hInject @_ @_ @Syntax <$> parseLiteral r)
-}

type Parser = Parsec Void Text

{- | inject to Coproduct of Syntax, then also inject to Product of [Syntax] (hSingleton)
| , then append metadata, finally wrap by HFix
-}
wrapper ::
  forall h h2 s m.
  ( HConstructive Metadata (HProduct '[Syntax]) h
  , HInjective h2 Syntax
  , MonadParsec s Text m
  ) =>
  Wapper m h2 (HFix h)
wrapper p = HFix <$> parseMetadata (hSingleton . (hInject @_ @h2 @Syntax) <$> p)

-- | Propagate metadata
propagate ::
  forall h h2 a.
  ( HConstructive Metadata (HProduct '[Syntax]) h
  , HInjective h2 Syntax
  , HExtractive Metadata h
  ) =>
  (HFix h a -> HFix h a -> h2 (HFix h) a) ->
  HFix h a ->
  HFix h a ->
  HFix h a
propagate f left right =
  let
    leftMeta = extractHFix left
    rightMeta = extractHFix right
    propagatedMeta = propagateMetadata leftMeta rightMeta
   in
    HFix $
      hConstruct propagatedMeta $
        hSingleton $
          hInject @_ @h2 @Syntax $
            f left right

parseAST :: forall x. (Typeable x) => Parser (AST x)
parseAST = tie $ \r ->
  quantify (parseLiteral wrapper r)
    <|> quantify (parseMType wrapper r)
    <|> quantify (parsePattern wrapper r)
    <|> quantify (parseDefinition wrapper r)
    <|> quantify (parseProgram wrapper r)
    <|> quantify (parseVariable wrapper r)
    <|> quantify (parseTypeVariable wrapper r)
    <|> quantify (parseExpr wrapper propagate r)
    <|> quantify (parseLet wrapper r)
    <|> quantify (parseBlock wrapper r)
