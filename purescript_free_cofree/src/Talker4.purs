module Talker4 where

import Prelude

import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Data.Functor.Coproduct (Coproduct, left, right)
import Effect (Effect)
import Effect.Console (log)

{- | Talk DSL -}
data TalkF r
  = Speak String r
  | Listen (String -> r)

type Talk a = Free TalkF a

speak :: String -> Talk Unit
speak s = liftF $ Speak s unit

listen :: Talk String
listen = liftF $ Listen identity

evalTalk :: String -> Talk ~> Effect
evalTalk name = foldFree interp
  where
    interp :: TalkF ~> Effect
    interp (Speak s next) = next <$ do
      log s
    interp (Listen reply) = do
      pure $ reply name

{- | Dinner DSL -}
data Food = Pizza | Chizburger
type IsThereMore = Boolean
type Bill = Int

data DinnerF next
  = Eat Food (IsThereMore -> next)
  | CheckPlease (Bill -> next)

type Dinner a = Free DinnerF a

eat :: Food -> Dinner IsThereMore
eat food = liftF $ Eat food identity

checkPlease :: Dinner Bill
checkPlease = liftF $ CheckPlease identity

{- | Coproduct Talk Dinner DSL -}
type LovelyEveningF = Coproduct TalkF DinnerF
type LovelyEvening = Free LovelyEveningF

liftTalk :: Talk ~> LovelyEvening
liftTalk = hoistFree left

liftDinner :: Dinner ~> LovelyEvening
liftDinner = hoistFree right

{- | Program -}

echo :: Talk Unit
echo = do
  speak "Hello"
  speak "What is your name ?"
  name <- listen
  speak ("Nice to meet you, " <> name)

dinnerTime :: LovelyEvening Unit
dinnerTime = do
  liftTalk $ speak "I'm famished!"
  isThereMore <- liftDinner $ eat Pizza
  if isThereMore
    then dinnerTime
    else do
      bill <- liftDinner checkPlease
      liftTalk $ speak "Outrageous!"

-- main :: Effect Unit
-- main = evalTalk "Jerry" echo
