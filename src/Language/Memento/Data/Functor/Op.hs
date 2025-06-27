{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Memento.Data.Functor.Op (
  Op (..),
  OpList,
  ReduceCoproduct (..),
) where

import           Language.Memento.Data.Functor.Coproduct (Coproduct, (:+:) (..))
import           Language.Memento.Data.Functor.Product   (Product, UnitF (..),
                                                          (:*:) (..))

-- | Wrapper to make Op work with Product
newtype Op f r a = Op (f a -> r)

-- | Type family to map a list of functors to a list of Op wrappers
type family OpList fs r where
  OpList '[] r = '[]
  OpList (f ': fs) r = Op f r ': OpList fs r

-- | Type class for reducing a coproduct to a result using handlers
class ReduceCoproduct fs r where
  reduceCoproduct :: Product (OpList fs r) a -> Coproduct fs a -> r

-- Base case: empty coproduct (VoidF)
instance ReduceCoproduct '[] r where
  reduceCoproduct UnitF voidF = case voidF of {}

-- Recursive case: non-empty coproduct
instance (ReduceCoproduct fs r) => ReduceCoproduct (f ': fs) r where
  reduceCoproduct (Op handler :*: handlers) coproduct = case coproduct of
    InjL fa   -> handler fa
    InjR rest -> reduceCoproduct @fs @r handlers rest
