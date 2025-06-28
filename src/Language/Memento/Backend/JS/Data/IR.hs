{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Language.Memento.Backend.JS.Data.IR (
  JSExpr (..),
  JSStmt (..),
  JSProgram (..),
  JSLiteral (..),
  JSUnaryOp (..),
  JSBinaryOp (..),
  JSIdentifier,
  JSSymbol (..),
) where

import Data.Text (Text)
import GHC.Base (List)

-- | JavaScript identifiers (variable names)
type JSIdentifier = Text

-- | JavaScript Symbol for data encoding - represents Symbol() values
data JSSymbol = JSSymbol 
  { symbolName :: Text        -- Description for Symbol(description)
  , symbolId :: Int          -- Unique ID for this symbol instance
  } deriving (Show, Eq, Ord)

-- | JavaScript literals
data JSLiteral
  = JSNumber Double
  | JSString Text
  | JSBoolean Bool
  | JSNull
  | JSUndefined
  | JSSymbolLit JSSymbol  -- Symbol("description")
  deriving (Eq, Ord, Show)

-- | JavaScript unary operators
data JSUnaryOp
  = JSNot      -- !
  | JSNegate   -- -
  | JSTypeOf   -- typeof
  deriving (Eq, Ord, Show)

-- | JavaScript binary operators
data JSBinaryOp
  = JSAdd         -- +
  | JSSubtract    -- -
  | JSMultiply    -- *
  | JSDivide      -- /
  | JSModulo      -- %
  | JSEqual       -- ===
  | JSNotEqual    -- !==
  | JSLessThan    -- <
  | JSGreaterThan -- >
  | JSLessEqual   -- <=
  | JSGreaterEqual -- >=
  | JSAnd         -- &&
  | JSOr          -- ||
  deriving (Eq, Ord, Show)

-- | JavaScript expressions
data JSExpr
  = JSLit JSLiteral
  | JSVar JSIdentifier
  | JSUnary JSUnaryOp JSExpr
  | JSBinary JSBinaryOp JSExpr JSExpr
  | JSCall JSExpr (List JSExpr)                    -- function call
  | JSMember JSExpr JSIdentifier                   -- obj.prop
  | JSIndex JSExpr JSExpr                          -- obj[expr]
  | JSArray (List JSExpr)                          -- [expr1, expr2, ...]
  | JSObject (List (JSIdentifier, JSExpr))         -- {prop1: expr1, prop2: expr2}
  | JSFunction (List JSIdentifier) (List JSStmt)   -- function(args) { stmts }
  | JSArrowFunction (List JSIdentifier) JSExpr     -- (args) => expr
  | JSConditional JSExpr JSExpr JSExpr             -- condition ? then : else
  | JSSymbolData JSSymbol (List JSExpr)            -- [symbol, arg1, arg2, ...] for data encoding
  deriving (Eq, Ord, Show)

-- | JavaScript statements
data JSStmt
  = JSExprStmt JSExpr                              -- expression;
  | JSVarDecl JSIdentifier (Maybe JSExpr)          -- var name = expr;
  | JSConst JSIdentifier JSExpr                    -- const name = expr;
  | JSLet JSIdentifier (Maybe JSExpr)              -- let name = expr;
  | JSAssign JSExpr JSExpr                         -- lvalue = rvalue;
  | JSIf JSExpr (List JSStmt) (Maybe (List JSStmt)) -- if (cond) { then } else { else }
  | JSWhile JSExpr (List JSStmt)                   -- while (cond) { body }
  | JSFor (Maybe JSStmt) (Maybe JSExpr) (Maybe JSExpr) (List JSStmt) -- for (init; cond; update) { body }
  | JSReturn (Maybe JSExpr)                        -- return expr;
  | JSThrow JSExpr                                 -- throw expr;
  | JSTryCatch (List JSStmt) JSIdentifier (List JSStmt) -- try { body } catch (e) { handler }
  | JSBreak                                        -- break;
  | JSContinue                                     -- continue;
  | JSBlock (List JSStmt)                          -- { stmts }
  deriving (Eq, Ord, Show)

-- | JavaScript program (top-level statements)
data JSProgram = JSProgram (List JSStmt)
  deriving (Eq, Ord, Show)