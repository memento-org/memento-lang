{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.Data.TypedAST (TyInfo, TypedAST, transformTyInfo, transformTypedAST) where
import           Language.Memento.Data.AST                       (Syntax)
import           Language.Memento.Data.AST.Metadata              (Metadata)
import           Language.Memento.Data.Functor.Coproduct.Higher  (HCoproduct,
                                                                  IsVoidIn (hAbsurd),
                                                                  (++:))
import           Language.Memento.Data.Functor.FixedPoint.Higher (HFix (HFix))
import           Language.Memento.Data.Functor.Higher            (HFunctor (hmap))
import           Language.Memento.Data.Functor.Product.Higher    (HExtractive (hExtract),
                                                                  HProduct,
                                                                  HUnit (HUnit),
                                                                  (:**:) (..))
import           Language.Memento.Data.TypedAST.TyInfo           (BlockTyInfo,
                                                                  DefinitionTyInfo,
                                                                  ExprTyInfo,
                                                                  LetTyInfo,
                                                                  LiteralTyInfo,
                                                                  PatternTyInfo,
                                                                  ProgramTyInfo,
                                                                  TypeTyInfo,
                                                                  TypeVariableTyInfo,
                                                                  VariableTyInfo,
                                                                  transformBlockTyInfo,
                                                                  transformDefinitionTyInfo,
                                                                  transformExprTyInfo,
                                                                  transformLetTyInfo,
                                                                  transformLiteralTyInfo,
                                                                  transformPatternTyInfo,
                                                                  transformProgramTyInfo,
                                                                  transformTypeTyInfo,
                                                                  transformTypeVariableTyInfo,
                                                                  transformVariableTyInfo)

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

-- Transformation functions

-- | Transform the TyInfo coproduct using the individual transformation functions
transformTyInfo :: (t -> t') -> TyInfo t f a -> TyInfo t' f' a
transformTyInfo ft =
  transformLiteralTyInfo ft ++:
  transformTypeTyInfo ft ++:
  transformExprTyInfo ft ++:
  transformDefinitionTyInfo ft ++:
  transformProgramTyInfo ft ++:
  transformTypeVariableTyInfo ft ++:
  transformPatternTyInfo ft ++:
  transformBlockTyInfo ft ++:
  transformLetTyInfo ft ++:
  transformVariableTyInfo ft ++:
  hAbsurd

-- | Transform an entire TypedAST from one type to another
transformTypedAST :: forall t t' a. (t -> t') -> TypedAST t a -> TypedAST t' a
transformTypedAST f = transformNode
  where
    transformNode :: TypedAST t a -> TypedAST t' a
    transformNode (HFix prod) =
      let tyInfo = hExtract prod :: TyInfo t (TypedAST t) a
          metadata = hExtract prod :: Metadata (TypedAST t) a
          syntax = hExtract prod :: Syntax (TypedAST t) a

          -- Transform the type info component
          transformedTyInfo = transformTyInfo f tyInfo

          -- Recursively transform child AST nodes in metadata and syntax
          transformedMetadata = hmap (transformTypedAST f) metadata
          transformedSyntax = hmap (transformTypedAST f) syntax

          -- Reconstruct the product with transformed components
          -- Build the product step by step: TyInfo t' :**: (Metadata :**: (Syntax :**: HUnit))
      in HFix $
        transformedTyInfo :**: transformedMetadata :**: transformedSyntax :**: HUnit
