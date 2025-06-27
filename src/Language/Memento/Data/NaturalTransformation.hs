{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Language.Memento.Data.NaturalTransformation (type (~>)) where

import           GHC.Base (Type)

-- Natural Transformation
type (f :: k -> Type) ~> (g :: k -> Type) = forall (a :: k). f a -> g a
