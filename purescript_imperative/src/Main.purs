module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (logShow)
import Data.Foldable (for_, foldM)
import Data.Traversable (traverse_)

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

main :: Effect Unit
main = do
  -- | for loop
  for_ [1,2,3] \i -> do
    logShow i

  -- | use for_ to control effect emission based on the case of Either and Maybe
  for_ (Left 4 :: Either Int Int) \i -> do
    logShow i -- skipped

  for_ (Right 5 :: Either Int Int) \i -> do
    logShow i

  for_ (Nothing :: Maybe Int) \i -> do
    logShow i -- skipped

  for_ (Just 6) \i -> do
    logShow i

  -- | traverse_ is better for function composition pipeline
  traverse_ (\i -> logShow i) $ Left 7 :: Either Int Int

  traverse_ (\i -> logShow i) $ Right 8 :: Either Int Int

  traverse_ (\i -> logShow i) $ Nothing :: Maybe Int

  traverse_ (\i -> logShow i) $ Just 9 :: Maybe Int

  -- | monadic fold
  traverse_ logShow $ foldM safeDivide 10000 [5, 2, 2] -- Just 500
  traverse_ logShow $ foldM safeDivide 10000 [5, 0, 2] -- Nothing



