{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Memento.Data.AST.Metadata (UniqueId, Metadata (..), propagateMetadata) where

import           Data.Kind                                   (Type)
import           Language.Memento.Data.Functor.Higher        (HFunctor (hmap))
import           Language.Memento.Data.NaturalTransformation (type (~>))
import           Text.Megaparsec                             (SourcePos)

type UniqueId = Int

-- | Type is a type-level function that maps a kind to a type.
data Metadata (f :: k -> Type) a where
  Metadata :: SourcePos -> SourcePos -> Metadata f a

propagateMetadata :: Metadata f a -> Metadata f a -> Metadata f a
propagateMetadata (Metadata left _) (Metadata _ right) = Metadata left right

deriving instance Show (Metadata f a)
deriving instance Eq (Metadata f a)
deriving instance Ord (Metadata f a)

instance HFunctor Metadata where
  hmap :: (f ~> g) -> Metadata f ~> Metadata g
  hmap _ = \case
    Metadata pos1 pos2 -> Metadata pos1 pos2
