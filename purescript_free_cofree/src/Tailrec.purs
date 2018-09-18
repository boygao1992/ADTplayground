module Tailrec where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Either (Either(..))

tailRec :: forall a b. (a -> Either a b) -> a -> b
tailRec f a = go (f a)
  where
    go :: Either a b -> b
    go (Left a') = go (f a')
    go (Right b) = b

tailRecM :: forall m a b. Monad m => (a -> m (Either a b)) -> a -> m b
tailRecM k a = go =<< k a
  where
    go :: Either a b -> m b
    go (Left a') = go =<< k a'
    go (Right b) = pure b

pow :: Int -> Int -> Int
pow n p = go (Tuple 1 p)
  where
    go (Tuple acc 0) = acc
    go (Tuple acc p') = go (Tuple (acc * n) (p' - 1))

pow' :: Int -> Int -> Int
pow' n p = tailRec powF (Tuple 1 p)
  where
    powF :: Tuple Int Int -> Either (Tuple Int Int) Int
    powF (Tuple acc 0) = Right acc
    powF (Tuple acc p') = Left $ Tuple (acc * n) (p' - 1)
