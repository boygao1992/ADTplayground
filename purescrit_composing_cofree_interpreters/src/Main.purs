module Main where

import Prelude

import Control.Comonad.Cofree (Cofree, explore, head, mkCofree, tail, unfoldCofree)
import Control.Monad.Free (Free, liftF)
import Data.Functor.Coproduct (Coproduct(..), left, right)
import Data.Functor.Product (Product(..), product)
import Effect (Effect)
import Effect.Console (log)


newtype StateA = StateA
  { count :: Int }
derive newtype instance showStateA :: Show StateA

data CommandA a = Add Int a
derive instance functorCommandA :: Functor CommandA

add :: Int -> Free CommandA (StateA -> StateA)
add x = liftF $ Add x identity

data RunCommandA a = RunCommandA
  { add :: Int -> a }

derive instance functorRunCommandA :: Functor RunCommandA

mkInterpA :: StateA -> Cofree RunCommandA StateA
mkInterpA state = unfoldCofree identity next state
  where
    add' :: StateA -> Int -> StateA
    add' (StateA st@ { count }) x = StateA (st { count = count + x })

    next :: StateA -> RunCommandA StateA
    next st = RunCommandA
      { add : add' st }

