{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Memento.Parser (parseAST) where

import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Base (Alternative (empty), Void)
import Language.Memento.Data.AST (AST, Syntax)
import Language.Memento.Data.AST.Metadata (Metadata)
import Language.Memento.Data.Functor.Combinator.Higher (quantify, tie, (<:>))
import Language.Memento.Data.Functor.Coproduct.Higher (Injective (hInject))
import Language.Memento.Data.Functor.FixedPoint.Higher (HFix (HFix))
import Language.Memento.Data.Functor.Product.Higher (Constructive, HProduct, hSingleton)
import Language.Memento.Parser.Definition (parseDefinition)
import Language.Memento.Parser.Literal (parseLiteral)
import Language.Memento.Parser.MType (parseMType)
import Language.Memento.Parser.Metadata (parseMetadata)
import Language.Memento.Parser.Pattern (parsePattern)
import Language.Memento.Parser.Program (parseProgram)
import Language.Memento.Parser.Variable (parseTypeVariable, parseVariable)
import Text.Megaparsec (MonadParsec, Parsec)

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

wrapper ::
  forall h h2 s m a.
  ( Constructive Metadata (HProduct '[Syntax]) h
  , Injective h2 Syntax
  , MonadParsec s Text m
  ) =>
  m (h2 (HFix h) a) ->
  m (HFix h a)
wrapper p = HFix <$> parseMetadata (hSingleton <$> hInject @_ @h2 @Syntax <$> p)

parseAST :: forall x. (Typeable x) => Parser (AST x)
parseAST = tie $ \r ->
  HFix
    <$> parseMetadata @(HProduct '[Syntax])
      ( hSingleton
          <$> ( quantify (parseLiteral r)
                  <:> quantify (parseMType wrapper r)
                  <:> quantify (parsePattern r)
                  <:> quantify (parseDefinition r)
                  <:> quantify (parseProgram r)
                  <:> quantify (parseVariable r)
                  <:> quantify (parseTypeVariable r)
                  <:> empty
              )
      )
