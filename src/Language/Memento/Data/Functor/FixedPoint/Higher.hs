{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Data.Functor.FixedPoint.Higher (HFix (..), extractHFix, injectHFix, projectHFix, safeProjectHFix) where

import Language.Memento.Data.Functor.Coproduct.Higher (Injective (hInject, hProject), SafeProjective (hSafeProject))
import Language.Memento.Data.Functor.Product.Higher (Extractive (hExtract))

newtype HFix h a = HFix {unHFix :: h (HFix h) a}

deriving instance (Show (h (HFix h) a)) => Show (HFix h a)
deriving instance (Eq (h (HFix h) a)) => Eq (HFix h a)
deriving instance (Ord (h (HFix h) a)) => Ord (HFix h a)

{-
  EXTRACT / INJECT
-}

extractHFix :: forall h h' a. (Extractive h h') => HFix h' a -> h (HFix h') a
extractHFix (HFix h) = hExtract h

injectHFix :: forall h h' a. (Injective h h') => h (HFix h') a -> HFix h' a
injectHFix h = HFix{unHFix = hInject h}

projectHFix :: forall h h' a. (Injective h h') => HFix h' a -> Maybe (h (HFix h') a)
projectHFix (HFix h) = hProject h

safeProjectHFix :: forall h h' a. (SafeProjective h h' a) => HFix h' a -> h (HFix h') a
safeProjectHFix (HFix h) = hSafeProject h
