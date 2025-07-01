{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Language.Memento.Data.Environment.Variance (Variance(..), composeVariance, (|*|), productVariance, combineVariance, (|+|), sumVariance, formatVariance) where

import           Data.Text (Text)

data Variance =
              Bivariant
              | Covariant
              | Contravariant
              | Invariant
  deriving (Show, Eq, Ord)

formatVariance :: Variance -> Text
formatVariance v = case v of
  Bivariant     -> "phantom"
  Covariant     -> "out"
  Contravariant -> "in"
  Invariant     -> "inout"

-- ComposeVariance x y is variance y in variance x: e.g., for function type (X => Y) => Z, X is covariant because contravariant (X) in contravariant (X => Y)
composeVariance :: Variance -> Variance -> Variance
composeVariance x y
  | x == Bivariant || y == Bivariant = Bivariant
  | x == Covariant = y
  | y == Covariant = x
  | x == Contravariant = flipVariance y
  | y == Contravariant = flipVariance x
  | otherwise = Invariant -- If both are invariant, result is invariant

-- Operator for composeVariance
infixl 4 |*|

(|*|) :: Variance -> Variance -> Variance
(|*|) = composeVariance

productVariance :: forall f. (Foldable f) => f Variance -> Variance
productVariance = foldr composeVariance Covariant

-- Combine variances when a type parameter appears in multiple positions
combineVariance :: Variance -> Variance -> Variance
combineVariance x y
  | x == Bivariant = y
  | y == Bivariant = x
  | x == Covariant && y == Covariant = Covariant
  | x == Contravariant && y == Contravariant = Contravariant
  | otherwise = Invariant -- One side is Invariant, or permutation of [Covariant, Contravairant]

-- Operator for combineVariance
infixl 4 |+|

(|+|) :: Variance -> Variance -> Variance
(|+|) = combineVariance

sumVariance :: forall f. (Foldable f) => f Variance -> Variance
sumVariance = foldr combineVariance Bivariant

-- Flip variance when entering contravariant position (like function arguments)
flipVariance :: Variance -> Variance
flipVariance v = case v of
  Covariant     -> Contravariant
  Contravariant -> Covariant
  Invariant     -> Invariant
  Bivariant     -> Bivariant
