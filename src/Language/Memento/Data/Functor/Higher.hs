{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

module Language.Memento.Data.Functor.Higher (HFunctor (..), HPhantom(..)) where

import           GHC.Base                                    (Type)
import           Language.Memento.Data.NaturalTransformation (type (~>))

class HFunctor (h :: (k -> Type) -> k -> Type) where
  hmap :: (f ~> g) -> h f ~> h g

class HPhantom (h :: (k -> Type) -> k -> Type) where
  hCoerce :: h f ~> h g
