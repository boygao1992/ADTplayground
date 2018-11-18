module Random.PseudoRandom.WithSeed where

import Prelude
import Random.PseudoRandom (class Random, RandomPair, Seed, random, randomR)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (State)
import Control.Monad.State (execState, get, modify_) as State
import Data.Array ((:))
import Data.Ord (abs)

type Result a =
  { values :: Array a
  , seed :: Seed
  }

type RandomsState a =
  { values :: Array a
  , n :: Int
  , seed :: Seed
  }

initRandomsState :: forall a. Int -> Seed -> RandomsState a
initRandomsState n seed = { values : [], n, seed }

-- tailRecM :: forall a b. (a -> m (Step a b)) -> a -> m b
-- a = Unit
-- b = Unit
-- m = State (RandomState _)
randomStep :: forall a. (Seed -> RandomPair a) -> Unit -> State (RandomsState a) (Step Unit Unit)
randomStep f _ = do
  { values, seed, n } <- State.get
  let
    { newVal, newSeed } = f seed
  if n == 0
    then
      pure $ Done unit
    else do
      State.modify_
        _ { values = (newVal : values)
          , seed = newSeed
          , n = n - 1
          }
      pure $ Loop unit

randomsFWithSeed :: forall a. (Seed -> RandomPair a) -> Int -> Seed -> Result a
randomsFWithSeed f n seed =
    (\state -> { values : state.values, seed : state.seed })
  $ State.execState (do
      tailRecM (randomStep f) unit
    )
    (initRandomsState (abs n) seed)

class Random a <= RandomsWithSeed a where
  randomsWithSeed :: Int -> Seed -> Result a
  randomRsWithSeed :: a -> a -> Int -> Seed -> Result a

instance randomsWithSeedRandom :: Random a => RandomsWithSeed a where
  randomsWithSeed :: Int -> Seed -> Result a
  randomsWithSeed n = randomsFWithSeed random n

  randomRsWithSeed :: a -> a -> Int -> Seed -> Result a
  randomRsWithSeed min max = randomsFWithSeed (randomR min max)
