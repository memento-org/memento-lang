{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Memento.TypeSolver.Subtype (isSubtype) where

import           Control.Monad                              (zipWithM)
import           Data.Map                                   (Map)
import qualified Data.Map                                   as Map
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.Text                                  (Text)
import           Language.Memento.Data.Environment.Ty       (TyCons)
import           Language.Memento.Data.Environment.Variance (Variance (..))
import           Language.Memento.Data.Functor.FixedPoint   (injectFix,
                                                             projectFix)
import           Language.Memento.Data.Ty                   (Literal (..), Ty,
                                                             TyF (..))
import           Language.Memento.TypeSolver.Utils          (isGeneric)
import           Language.Memento.Typing.Core               (TypingError (..))
import           Language.Memento.Util                      (someM)

-- | Check if t1 is a subtype of t2
isSubtype :: TyCons Variance -> Map Text (Set Ty, Set Ty) -> Ty -> Ty -> Either TypingError Bool
isSubtype variances bounds = go
  where
    go :: Ty -> Ty -> Either TypingError Bool
    go ty1 ty2 = -- traceShow (ty1, ty2) $
     case (projectFix ty1, projectFix ty2) of
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
      (TLiteral (LNumber _), TNumber) -> Right True  -- Any literal number is subtype of number
      (TLiteral (LInt _), TInt) -> Right True     -- Any literal int is subtype of int
      (TLiteral (LBool _), TBool) -> Right True    -- Any literal bool is subtype of bool
      (TLiteral (LString _), TString) -> Right True  -- Any literal string is subtype of string
      (TLiteral l1, TLiteral l2) -> Right (l1 == l2)

      -- Int & Number
      (TInt, TNumber) -> Right True
      (TLiteral (LInt _), TNumber) -> Right True

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
        results <- mapM (`go` injectFix t2F) ts1
        return $ and results

      -- Intersection subtyping: t1 <: TIntersection ts2 iff for all ts in ts2, t1 <: ts
      (_, TIntersection ts2) -> do
        results <- mapM (go $ injectFix t1F) ts2
        return $ and results

      -- t1 <: TUnion ts2 iff exists ts in ts2 such that t1 <: ts
      (_, TUnion ts2) -> do
        results <- mapM (go $ injectFix t1F) ts2
        return $ or results

      -- TIntersection ts1 <: t2 iff exists ts in ts1 such that ts <: t2
      (TIntersection ts1, _) -> do
        results <- mapM (`go` injectFix t2F) ts1
        return $ or results

      -- Generic subtyping
      -- ジェネリクスの連鎖は本来はグラフでやるべき
      -- 今は一段階、もしくは具体的な上界と下界のみで判断している
      (TGeneric name1, TGeneric name2)
        | name1 == name2 -> Right True
        | otherwise -> do
            let (_, upper1) = lookupGenericBounds name1 bounds
            let (lower2, _) = lookupGenericBounds name2 bounds
            let tGeneric2 = injectFix $ TGeneric name2
            let tGeneric1 = injectFix $ TGeneric name1
            let t1F' = injectFix t1F
            let t2F' = injectFix t2F
            let easyCheck = Set.member tGeneric2 upper1 || Set.member tGeneric1 lower2
            if easyCheck
              then Right True
              else do
                    let upper1WithoutTGeneric = Set.filter (not . isGeneric) upper1
                    let lower2WithoutTGeneric = Set.filter (not . isGeneric) lower2
                    results1 <- someM (`go` t2F') $ Set.toList upper1WithoutTGeneric
                    results2 <- someM (go t1F') $ Set.toList lower2WithoutTGeneric
                    return $ results1 || results2

      (TGeneric name, _) -> do
        let (_, upper) = lookupGenericBounds name bounds
        someM (`go` injectFix t2F) $ Set.toList $ Set.filter (not . isGeneric) upper

      (_, TGeneric name) -> do
        let (lower, _) = lookupGenericBounds name bounds
        someM (go $ injectFix t1F) $ Set.toList $ Set.filter (not . isGeneric) lower

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

-- | Lookup generic bounds, defaulting to (never, unknown)
lookupGenericBounds :: Text -> Map Text (Set Ty, Set Ty) -> (Set Ty, Set Ty)
lookupGenericBounds = Map.findWithDefault (Set.empty, Set.empty)
