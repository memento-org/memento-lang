module Language.Memento.Data.Environment.Ty (TyCons, TyConsConstructor(..), TyGenerics) where

{-
Type Environments
-}

import           Data.Map                 (Map)
import           Data.Set                 (Set)
import           Data.Text                (Text)
import           GHC.Base                 (List)
import           Language.Memento.Data.Ty (Ty)

-- | Type constructors' names and their variances & value constructor.
-- | v represents types for variances
type TyCons v = Map Text (List v, List TyConsConstructor)

-- | Value constructor for type constructors
-- | Example:
-- |  data WeirdOpt<auto> { Some<T>(x : T | number) -> WeirdOpt<T & bool> }
-- | then
-- |  tccName = "Some"
-- |  tccGenerics = ["T"]
-- |  tccArgs = [(T | number)]
-- |  tccReturn = [T & bool]
data TyConsConstructor = TyConsConstructor
  { tccName     :: Text -- ^ value constructor name
  , tccGenerics :: List Text -- ^ value constructor generics
  , tccArgs     :: List Ty -- ^ value constructor arguments
  , tccReturn   :: List Ty -- ^ value constructor return type's type arguments
  } deriving (Show, Eq, Ord)

-- | Current type generics
type TyGenerics = Set Text
