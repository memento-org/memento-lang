{-# LANGUAGE KindSignatures #-}

module Language.Memento.Data.AST.Tag (
  KLiteral,
  KVariable,
  KTypeVariable,
  KType,
  KDefinition,
  KExpr,
  KPattern,
  KProgram,
  KLet,
  KBlock,
) where

import           Data.Data (Typeable)
import           Data.Kind (Type)

{- Literals -}
data KLiteral :: Type deriving (Typeable)

{- Variables -}
data KVariable :: Type deriving (Typeable)
data KTypeVariable :: Type deriving (Typeable)

{- Types -}
data KType :: Type deriving (Typeable)

-- data KEffects :: Type

{- Definitions -}
data KDefinition :: Type deriving (Typeable)

{- Expressions -}
data KLet :: Type deriving (Typeable)
data KExpr :: Type deriving (Typeable)
data KBlock :: Type deriving (Typeable)
data KPattern :: Type deriving (Typeable)

-- data KHandlerClause :: Type

{- Programs -}
data KProgram :: Type deriving (Typeable)
