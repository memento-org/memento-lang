{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Memento.Data.Type.NonEq (type (/~)) where

import GHC.Base (Constraint, Type)
import GHC.TypeError (
  ErrorMessage (ShowType, Text, (:<>:)),
  TypeError,
 )

type family (a :: Type) /~ (b :: Type) :: Constraint where
  a /~ a =
    TypeError
      ( 'Text "Type "
          ':<>: 'ShowType a
          ':<>: 'Text " is not allowed to be equal to itself."
      )
  _ /~ _ = () -- 任意の他の型は制約なし
