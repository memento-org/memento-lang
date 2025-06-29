{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Memento.Typing.Scheme (
  instantiateScheme,
  instantiateSchemeWithGenerics
) where

import           Language.Memento.Data.Ty           (TypeScheme (..), UnsolvedTy,
                                                     assignTypeToScheme)
import           Language.Memento.Typing.Core       (TypingM, freshGeneric,
                                                     freshTyVar)

-- | Instantiate a type scheme with fresh type variables
instantiateScheme :: TypeScheme -> TypingM UnsolvedTy
instantiateScheme (TypeScheme generics ty) = do
  -- Instantiate generics with fresh type variables
  freshVars <- mapM (const freshTyVar) generics
  let instantiated = assignTypeToScheme freshVars (TypeScheme generics ty)
  return instantiated

-- | Instantiate a type scheme with fresh generic variables
instantiateSchemeWithGenerics :: TypeScheme -> TypingM UnsolvedTy
instantiateSchemeWithGenerics (TypeScheme generics ty) = do
  -- Instantiate generics with fresh generic variables
  freshGens <- mapM (const freshGeneric) generics
  let instantiated = assignTypeToScheme freshGens (TypeScheme generics ty)
  return instantiated