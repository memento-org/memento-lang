module Language.Memento.Util (partitionFirst, untilSuccess, someM, allM, orM, andM) where
import           Control.Monad.Error.Class (MonadError (catchError))


partitionFirst :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
partitionFirst f' xs' = go f' xs' []
 where
  go _ [] _ = Nothing -- No match found
  go f (x : xs) acc =
    case f x of
      Just b  -> Just (b, acc ++ xs) -- Found a match, return it with remaining elements
      Nothing -> go f xs (x : acc) -- Continue searching

-- | Run a function until it succeeds or all inputs are exhausted
untilSuccess :: (MonadError e m) => [a] -> (a -> m b) -> m (Either [e] b)
untilSuccess xs f = case xs of
  [] -> pure $ Left []
  (x : xs') -> do
    result <- (Right <$> f x) `catchError` \e -> pure $ Left e
    case result of
      Right v -> pure $ Right v -- Success, return the value
      Left e -> do
        restResult <- untilSuccess xs' f -- Try the rest of the inputs
        case restResult of
          Right v -> pure $ Right v -- Found a success in the rest
          Left es -> pure $ Left (e : es) -- Collect errors

someM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
someM _ [] = pure False
someM f (x : xs) = do
  result <- f x
  if result
    then pure True
    else someM f xs

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = pure True
allM f (x : xs) = do
  result <- f x
  if result
    then allM f xs
    else pure False

orM :: (Monad m) => [m Bool] -> m Bool
orM = someM id

andM :: (Monad m) => [m Bool] -> m Bool
andM = allM id
