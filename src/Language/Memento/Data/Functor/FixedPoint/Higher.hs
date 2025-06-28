{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Memento.Data.Functor.FixedPoint.Higher (HFix (..), extractHFix, injectHFix, projectHFix, safeProjectHFix, safeProjectVia) where

import           Language.Memento.Data.Functor.Coproduct.Higher (HInjective (hInject, hProject),
                                                                 SafeHProjective (hSafeProject))
import           Language.Memento.Data.Functor.Product.Higher   (HExtractive (hExtract))

newtype HFix h a = HFix {unHFix :: h (HFix h) a}

deriving instance (Show (h (HFix h) a)) => Show (HFix h a)
deriving instance (Eq (h (HFix h) a)) => Eq (HFix h a)
deriving instance (Ord (h (HFix h) a)) => Ord (HFix h a)

{-
  EXTRACT / INJECT
-}

extractHFix :: forall h h' a. (HExtractive h h') => HFix h' a -> h (HFix h') a
extractHFix (HFix h) = hExtract h

injectHFix :: forall h h' a. (HInjective h h') => h (HFix h') a -> HFix h' a
injectHFix h = HFix{unHFix = hInject h}

projectHFix :: forall h h' a. (HInjective h h') => HFix h' a -> Maybe (h (HFix h') a)
projectHFix (HFix h) = hProject h

safeProjectHFix :: forall h h' a. (SafeHProjective h h' a) => HFix h' a -> h (HFix h') a
safeProjectHFix (HFix h) = hSafeProject h

safeProjectVia :: forall hVia h h' a. (HExtractive hVia h, SafeHProjective h' hVia a) => HFix h a -> h' (HFix h) a
safeProjectVia ast = hSafeProject @h' $ extractHFix @hVia ast
