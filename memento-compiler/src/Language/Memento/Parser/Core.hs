{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.Parser.Core (
  Parser,
  sc,
  parseLexeme,
  parseIsolatedSymbol,
  parseParens,
  parseBrackets,
  parseBraces,
  parseAngleBrackets,
  reservedWords,
  parseReservedWord,
  parseIdentifier,
  parseSymbol,
) where

import qualified Control.Monad.State        as State
import           Data.Functor               (void)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (notFollowedBy, try),
                                             ParsecT, between, many, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             punctuationChar, space1, string,
                                             symbolChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text (State.State Int)

-- | スペース消費パーサー
sc :: (MonadParsec s Text m) => m ()
sc =
  L.space
    space1 -- スペース、タブ、改行を消費
    (L.skipLineComment "//") -- 行コメント
    (L.skipBlockComment "/*" "*/") -- ブロックコメント

-- | レキサー
parseLexeme :: (MonadParsec s Text m) => m a -> m a
parseLexeme = L.lexeme sc

parseSymbol :: (MonadParsec s Text m) => Text -> m ()
parseSymbol n =
  void $ try $ L.symbol sc n

parseIsolatedSymbol :: (MonadParsec s Text m) => Text -> m ()
parseIsolatedSymbol n =
  void $ try $ parseLexeme $ string n <* notFollowedBy (symbolChar <|> punctuationChar)

-- | 括弧で囲まれた式
parseParens :: (MonadParsec s Text m) => m a -> m a
parseParens = between (L.symbol sc "(") (L.symbol sc ")")

-- | 角括弧で囲まれた式
parseBrackets :: (MonadParsec s Text m) => m a -> m a
parseBrackets = between (L.symbol sc "[") (L.symbol sc "]")

-- | 中括弧で囲まれた式
parseBraces :: (MonadParsec s Text m) => m a -> m a
parseBraces = between (L.symbol sc "{") (L.symbol sc "}")

-- | 角度括弧で囲まれた式 (型パラメータ用)
parseAngleBrackets :: (MonadParsec s Text m) => m a -> m a
parseAngleBrackets = between (L.symbol sc "<") (L.symbol sc ">")

-- | 予約語のリスト
reservedWords :: [Text]
reservedWords =
  [ -- expression
    "if"
  , "then"
  , "else"
  , "switch"
  , -- literal
    "true"
  , "false"
  , -- primitive types
    "number"
  , "int"
  , "bool"
  , "string"
  , "unknown"
  , "never"
  , -- definitions
    "val"
  , "fn"
  , "let"
  , "data"
  , -- (Future work) effects
    "with"
  , "effect"
  , "handle"
  -- variances
  , "auto"
  , "in"
  , "out"
  , "inout"
  , "phantom"
  -- javascript reserved words
  {-
  break    case      catch     class     const
continue debugger  default   delete    do
else     export    extends   finally   for
function if        import    in        instanceof
new      return    super     switch    this
throw    try       typeof    var       void
while    with      yield
  -}
  , "break"
  , "case"
  , "catch"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "else"
  , "export"
  , "extends"
  , "finally"
  , "for"
  , "function"
  , "if"
  , "import"
  , "in"
  , "instanceof"
  , "new"
  , "return"
  , "super"
  , "switch"
  , "this"
  , "throw"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "while"
  , "with"
  , "yield"
  -- Javascript literal
  , "null"
  , "true"
  , "false"
  -- Javascript contextual keywords
  , "await"
  , "let"
  , "yield"
  -- Javascript future reserved words
  , "enum"
  , "implements"
  , "package"
  , "private"
  , "protected"
  , "public"
  , "static"
  , "interface"
  -- Javascript no-longer reserved words, but here we keep them for compatibility
  , "abstract"
  , "boolean"
  , "byte"
  , "char"
  , "double"
  , "final"
  , "float"
  , "goto"
  , "int"
  , "long"
  , "native"
  , "short"
  , "synchronized"
  , "throws"
  , "transient"
  , "volatile"
  ]

-- | 予約語
parseReservedWord :: (MonadParsec s Text m) => Text -> m ()
parseReservedWord w = try $ parseLexeme (string w *> notFollowedBy (alphaNumChar <|> char '_'))

-- | 識別子
parseIdentifier :: (MonadParsec s Text m, MonadFail m) => m Text
parseIdentifier = try $ parseLexeme $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
  if name `elem` reservedWords
    then fail $ "keyword " <> show name <> " cannot be an identifier"
    else return name
