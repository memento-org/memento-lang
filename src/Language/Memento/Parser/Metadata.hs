{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Memento.Parser.Metadata (parseMetadata) where

import Data.Text (Text)
import Language.Memento.Data.AST.Metadata (Metadata (Metadata))
import Language.Memento.Data.Functor.Product.Higher (Constructive (hConstruct))
import Text.Megaparsec (MonadParsec, getSourcePos)

parseMetadata ::
  (Constructive Metadata h h', MonadParsec s Text m) => m (h f a) -> m (h' f a)
parseMetadata p = do
  startPos <- getSourcePos
  a <- p
  endPos <- getSourcePos
  return $ hConstruct (Metadata startPos endPos) a

-- propagateMetadata :: Metadata f a -> Metadata f b -> Metadata f c
-- propagateMetadata (Metadata startPos1 _) (Metadata _ endPos2) = Metadata startPos1 endPos2
