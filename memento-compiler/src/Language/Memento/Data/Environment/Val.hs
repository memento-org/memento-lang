module Language.Memento.Data.Environment.Val (ValDefs) where

import           Data.Map                 (Map)
import           Data.Text                (Text)
import           Language.Memento.Data.Ty (TypeScheme)

-- | Value Definitions (names and their type schemes)
type ValDefs = Map Text TypeScheme
