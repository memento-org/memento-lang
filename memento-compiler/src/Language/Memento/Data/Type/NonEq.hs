{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Data.Type.NonEq (
  type (/~),
) where

import           Data.Kind    (Constraint)
import           GHC.TypeLits (ErrorMessage (..), TypeError)

type family (/~) (a :: k) (b :: k) :: Constraint where
  a /~ a = TypeError ('Text "Types are equal: " ':<>: 'ShowType a ':<>: 'Text " ~ " ':<>: 'ShowType a)
  a /~ b = ()
