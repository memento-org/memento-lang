{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.Subtype (isSubtype) where

import           Control.Monad                              (zipWithM)
import           Data.Map                                   (Map)
import qualified Data.Map                                   as Map
import           Data.Text                                  (Text)
import           Language.Memento.Data.Environment.Ty       (TyCons)
import           Language.Memento.Data.Environment.Variance (Variance (..))
import           Language.Memento.Data.Functor.FixedPoint   (injectFix,
                                                             projectFix)
import           Language.Memento.Data.Ty                   (Ty, TyF (..))
import           Language.Memento.Typing.Core               (TypingError (..))

-- | Check if t1 is a subtype of t2
isSubtype :: TyCons Variance -> Map Text (Ty, Ty) -> Ty -> Ty -> Either TypingError Bool
isSubtype variances bounds t1 t2 = go t1 t2
  where
    go :: Ty -> Ty -> Either TypingError Bool
    go ty1 ty2 = case (projectFix ty1, projectFix ty2) of
      (Just t1F, Just t2F) -> subtypeCheck t1F t2F
      _                    -> Right False  -- Should not happen for Ty since it only contains TyF

    subtypeCheck :: TyF Ty -> TyF Ty -> Either TypingError Bool
    subtypeCheck t1F t2F = case (t1F, t2F) of
      -- Base cases
      (_, TUnknown) -> Right True
      (TNever, _) -> Right True

      -- Equality cases
      (TNumber, TNumber) -> Right True
      (TInt, TInt) -> Right True
      (TBool, TBool) -> Right True
      (TString, TString) -> Right True

      -- Literal subtyping - using wildcards since Literal constructors are not exported
      (TLiteral _, TNumber) -> Right True  -- Any literal number is subtype of number
      (TLiteral _, TInt) -> Right True     -- Any literal int is subtype of int
      (TLiteral _, TBool) -> Right True    -- Any literal bool is subtype of bool
      (TLiteral _, TString) -> Right True  -- Any literal string is subtype of string
      (TLiteral l1, TLiteral l2) -> Right (l1 == l2)

      -- Function subtyping (contravariant in arguments, covariant in return)
      (TFunction args1 ret1, TFunction args2 ret2)
        | length args1 == length args2 -> do
            -- Arguments are contravariant: args2 <: args1
            argResults <- zipWithM go args2 args1
            -- Return type is covariant: ret1 <: ret2
            retResult <- go ret1 ret2
            return $ and argResults && retResult
        | otherwise -> Right False

      -- Union subtyping: TUnion ts1 <: t2 iff all ts in ts1, ts <: t2
      (TUnion ts1, _) -> do
        results <- mapM (`go` t2) ts1
        return $ and results

      -- Intersection subtyping: t1 <: TIntersection ts2 iff for all ts in ts2, t1 <: ts
      (_, TIntersection ts2) -> do
        results <- mapM (go t1) ts2
        return $ and results

      -- t1 <: TUnion ts2 iff exists ts in ts2 such that t1 <: ts
      (_, TUnion ts2) -> do
        results <- mapM (go t1) ts2
        return $ or results

      -- TIntersection ts1 <: t2 iff exists ts in ts1 such that ts <: t2
      (TIntersection ts1, _) -> do
        results <- mapM (`go` t2) ts1
        return $ or results

      -- Generic subtyping
      (TGeneric name1, TGeneric name2)
        | name1 == name2 -> Right True
        | otherwise -> do
            let (_, upper1) = lookupGenericBounds name1 bounds
            let (lower2, _) = lookupGenericBounds name2 bounds
            let tGeneric2 = injectFix $ TGeneric name2
            let tGeneric1 = injectFix $ TGeneric name1
            -- Check if name2 is in upper bounds of name1 OR name1 is in lower bounds of name2
            return $ upper1 == tGeneric2 || lower2 == tGeneric1

      (TGeneric name, _) -> do
        let (_, upper) = lookupGenericBounds name bounds
        go upper t2

      (_, TGeneric name) -> do
        let (lower, _) = lookupGenericBounds name bounds
        go t1 lower

      -- Type application subtyping
      (TApplication name1 args1, TApplication name2 args2)
        | name1 /= name2 -> Right False
        | length args1 /= length args2 -> Right False
        | otherwise -> checkArgumentsWithVariance name1 args1 args2

      -- Default case
      _ -> Right False

    checkArgumentsWithVariance :: Text -> [Ty] -> [Ty] -> Either TypingError Bool
    checkArgumentsWithVariance name args1 args2 = case Map.lookup name variances of
      Nothing           -> Left $ UndefinedTypeConstructor name Nothing
      Just (varList, _) -> checkArgsWithVarList varList args1 args2

    checkArgsWithVarList :: [Variance] -> [Ty] -> [Ty] -> Either TypingError Bool
    checkArgsWithVarList [] [] [] = Right True
    checkArgsWithVarList (v:vs) (a1:a1s) (a2:a2s) = do
      let check = case v of
            Covariant     -> go a1 a2
            Contravariant -> go a2 a1
            Invariant     -> do
              r1 <- go a1 a2
              r2 <- go a2 a1
              return (r1 && r2)
            Bivariant     -> Right True
      result <- check
      rest <- checkArgsWithVarList vs a1s a2s
      return $ result && rest
    checkArgsWithVarList _ _ _ = Right False

-- | Create a TNever type
tNever :: Ty
tNever = injectFix TNever

-- | Create a TUnknown type
tUnknown :: Ty
tUnknown = injectFix TUnknown

-- | Lookup generic bounds, defaulting to (never, unknown)
lookupGenericBounds :: Text -> Map Text (Ty, Ty) -> (Ty, Ty)
lookupGenericBounds = Map.findWithDefault (tNever, tUnknown)
