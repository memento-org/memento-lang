{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Memento.Data.Functor.Combinator.Higher (quantify, (<:>), tie, Family, Wrapper) where

import           Control.Applicative                            (Alternative (empty))
import           Data.Typeable                                  (Typeable, eqT,
                                                                 type (:~:) (Refl))
import           GHC.Base                                       (Alternative ((<|>)))
import           Language.Memento.Data.Functor.Coproduct.Higher (HInjective (hInject))

type Family m f = forall x. (Typeable x) => m (f x)

type Wrapper m h f = forall a. m (h f a) -> m (f a)

-- | Extend a process (exclusively parser) from some type `a` to all types `x`. (if a /= x, then empty)
quantify ::
  forall m f a.
  (Alternative m, Typeable a) =>
  m (f a) ->
  Family m f
quantify m = m'
 where
  m' :: forall x. (Typeable x) => m (f x)
  m' =
    case eqT @a @x of
      Just Refl -> m
      Nothing   -> empty

infixr 4 <:>

-- | Cons a branch to a parser
(<:>) ::
  forall h h' m f.
  (Alternative m, HInjective h h') =>
  Family m (h f) ->
  Family m (h' f) ->
  Family m (h' f)
m1 <:> m2 = hInject <$> m1 <|> m2

-- | Recursively call a parser
tie ::
  forall m f a.
  (Typeable a) =>
  (Family m f -> Family m f) ->
  m (f a)
tie f = f (tie f)
