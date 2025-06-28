{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Backend.JS.Codegen (
  formatJSExpr,
  formatJSStmt,
  formatJSProgram,
  formatJSLiteral,
  formatJSUnaryOp,
  formatJSBinaryOp,
  formatJSSymbol,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Memento.Backend.JS.Data.IR

-- | Format JavaScript literals
formatJSLiteral :: JSLiteral -> Text
formatJSLiteral = \case
  JSNumber n -> T.pack (show n)
  JSString s -> "\"" <> T.replace "\"" "\\\"" s <> "\""
  JSBoolean True -> "true"
  JSBoolean False -> "false"
  JSNull -> "null"
  JSUndefined -> "undefined"
  JSSymbolLit symbol -> formatJSSymbol symbol

-- | Format JavaScript Symbol constructor
formatJSSymbol :: JSSymbol -> Text
formatJSSymbol (JSSymbol name _) = "Symbol(\"" <> name <> "\")"

-- | Format JavaScript unary operators
formatJSUnaryOp :: JSUnaryOp -> Text
formatJSUnaryOp = \case
  JSNot -> "!"
  JSNegate -> "-"
  JSTypeOf -> "typeof "

-- | Format JavaScript binary operators
formatJSBinaryOp :: JSBinaryOp -> Text
formatJSBinaryOp = \case
  JSAdd -> " + "
  JSSubtract -> " - "
  JSMultiply -> " * "
  JSDivide -> " / "
  JSModulo -> " % "
  JSEqual -> " === "
  JSNotEqual -> " !== "
  JSLessThan -> " < "
  JSGreaterThan -> " > "
  JSLessEqual -> " <= "
  JSGreaterEqual -> " >= "
  JSAnd -> " && "
  JSOr -> " || "

-- | Format JavaScript expressions
formatJSExpr :: JSExpr -> Text
formatJSExpr = \case
  JSLit lit -> formatJSLiteral lit
  JSVar name -> name
  JSUnary op expr -> formatJSUnaryOp op <> formatJSExpr expr
  JSBinary op left right -> 
    "(" <> formatJSExpr left <> formatJSBinaryOp op <> formatJSExpr right <> ")"
  JSCall func args -> 
    formatJSExpr func <> "(" <> T.intercalate ", " (map formatJSExpr args) <> ")"
  JSMember obj prop -> formatJSExpr obj <> "." <> prop
  JSIndex obj index -> formatJSExpr obj <> "[" <> formatJSExpr index <> "]"
  JSArray elements -> 
    "[" <> T.intercalate ", " (map formatJSExpr elements) <> "]"
  JSObject props -> 
    "{" <> T.intercalate ", " (map formatProp props) <> "}"
    where formatProp (key, value) = key <> ": " <> formatJSExpr value
  JSFunction params body -> 
    "function(" <> T.intercalate ", " params <> ") {\n" <> 
    T.intercalate "\n" (map formatJSStmt body) <> "\n}"
  JSArrowFunction params expr -> 
    "(" <> T.intercalate ", " params <> ") => " <> formatJSExpr expr
  JSConditional cond then_ else_ -> 
    "(" <> formatJSExpr cond <> " ? " <> formatJSExpr then_ <> " : " <> formatJSExpr else_ <> ")"
  JSSymbolData symbol args -> 
    "[" <> formatJSSymbol symbol <> 
    (if null args then "" else ", " <> T.intercalate ", " (map formatJSExpr args)) <> "]"

-- | Format JavaScript statements
formatJSStmt :: JSStmt -> Text
formatJSStmt = \case
  JSExprStmt expr -> formatJSExpr expr <> ";"
  JSVarDecl name Nothing -> "var " <> name <> ";"
  JSVarDecl name (Just expr) -> "var " <> name <> " = " <> formatJSExpr expr <> ";"
  JSConst name expr -> "const " <> name <> " = " <> formatJSExpr expr <> ";"
  JSLet name Nothing -> "let " <> name <> ";"
  JSLet name (Just expr) -> "let " <> name <> " = " <> formatJSExpr expr <> ";"
  JSAssign lval rval -> formatJSExpr lval <> " = " <> formatJSExpr rval <> ";"
  JSIf cond then_ Nothing -> 
    "if (" <> formatJSExpr cond <> ") {\n" <> 
    T.intercalate "\n" (map formatJSStmt then_) <> "\n}"
  JSIf cond then_ (Just else_) -> 
    "if (" <> formatJSExpr cond <> ") {\n" <> 
    T.intercalate "\n" (map formatJSStmt then_) <> "\n} else {\n" <>
    T.intercalate "\n" (map formatJSStmt else_) <> "\n}"
  JSWhile cond body -> 
    "while (" <> formatJSExpr cond <> ") {\n" <> 
    T.intercalate "\n" (map formatJSStmt body) <> "\n}"
  JSFor initStmt cond update body -> 
    "for (" <> 
    maybe "" (T.dropEnd 1 . formatJSStmt) initStmt <> "; " <>
    maybe "" formatJSExpr cond <> "; " <>
    maybe "" formatJSExpr update <> 
    ") {\n" <> T.intercalate "\n" (map formatJSStmt body) <> "\n}"
  JSReturn Nothing -> "return;"
  JSReturn (Just expr) -> "return " <> formatJSExpr expr <> ";"
  JSThrow expr -> "throw " <> formatJSExpr expr <> ";"
  JSTryCatch body var handler -> 
    "try {\n" <> T.intercalate "\n" (map formatJSStmt body) <> 
    "\n} catch (" <> var <> ") {\n" <> 
    T.intercalate "\n" (map formatJSStmt handler) <> "\n}"
  JSBreak -> "break;"
  JSContinue -> "continue;"
  JSBlock stmts -> 
    "{\n" <> T.intercalate "\n" (map formatJSStmt stmts) <> "\n}"

-- | Format JavaScript program
formatJSProgram :: JSProgram -> Text
formatJSProgram (JSProgram stmts) = T.intercalate "\n\n" (map formatJSStmt stmts)