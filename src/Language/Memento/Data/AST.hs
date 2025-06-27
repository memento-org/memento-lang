{-# LANGUAGE DataKinds #-}

module Language.Memento.Data.AST (Syntax, AST) where

import           Language.Memento.Data.AST.Definition            (Definition)
import           Language.Memento.Data.AST.Expr                  (Block, Expr,
                                                                  Let)
import           Language.Memento.Data.AST.Literal               (Literal)
import           Language.Memento.Data.AST.Metadata              (Metadata)
import           Language.Memento.Data.AST.MType                 (MType)
import           Language.Memento.Data.AST.Pattern               (Pattern)
import           Language.Memento.Data.AST.Program               (Program)
import           Language.Memento.Data.AST.Variable              (TypeVariable,
                                                                  Variable)
import           Language.Memento.Data.Functor.Coproduct.Higher  (HCoproduct)
import           Language.Memento.Data.Functor.FixedPoint.Higher (HFix)
import           Language.Memento.Data.Functor.Product.Higher    (HProduct)

type Syntax =
  HCoproduct
    [ Literal
    , MType
    , Expr
    , Definition
    , Program
    , Variable
    , TypeVariable
    , Pattern
    , Let
    , Block
    ]

type AST = HFix (HProduct [Metadata, Syntax])
