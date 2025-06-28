{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Language.Memento.Data.TypedAST (TyInfo, TypedAST) where
import           Language.Memento.Data.AST                       (Syntax)
import           Language.Memento.Data.AST.Metadata              (Metadata)
import           Language.Memento.Data.Functor.Coproduct.Higher  (HCoproduct)
import           Language.Memento.Data.Functor.FixedPoint.Higher (HFix)
import           Language.Memento.Data.Functor.Product.Higher    (HProduct)
import           Language.Memento.Data.TypedAST.TyInfo           (BlockTyInfo,
                                                                  DefinitionTyInfo,
                                                                  ExprTyInfo,
                                                                  LetTyInfo,
                                                                  LiteralTyInfo,
                                                                  PatternTyInfo,
                                                                  ProgramTyInfo,
                                                                  TypeTyInfo,
                                                                  TypeVariableTyInfo,
                                                                  VariableTyInfo)

-- | Type information for AST node
-- | `t` represents type of type (Huh?) (Haskell type represents type of Memento types)
type TyInfo t = HCoproduct '[
  LiteralTyInfo t
  , TypeTyInfo t
  , ExprTyInfo t
  , DefinitionTyInfo  t
  , ProgramTyInfo t
  , TypeVariableTyInfo t
  , PatternTyInfo t
  , BlockTyInfo t
  , LetTyInfo t
  , VariableTyInfo t
  ]

-- | Each node has type information.
type TypedAST t = HFix (HProduct '[TyInfo t, Metadata, Syntax])
