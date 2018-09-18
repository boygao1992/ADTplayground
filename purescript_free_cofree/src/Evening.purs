module Evening where

import Prelude

import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Run (SProxy(..), FProxy, Step(..), Run, EFFECT, liftEffect)
import Run as Run

{- | Talk DSL -}
data TalkF next
  = Speak String next
  | Listen (String -> next)

derive instance functorTalkF :: Functor TalkF

type TALK = FProxy TalkF

_talk = SProxy :: SProxy "talk"

speak :: forall r. String -> Run (talk :: TALK | r) Unit
speak str = Run.lift _talk $ Speak str unit

listen :: forall r. Run (talk :: TALK | r) String
listen = Run.lift _talk $ Listen identity

handleTalk :: forall r. TalkF ~> Run (effect :: EFFECT | r)
handleTalk (Speak str next) = do
  liftEffect $ Console.log str
  pure next
handleTalk (Listen reply) = do
  pure $ reply "answer"

evalTalk
  :: forall r
   . Run (effect :: EFFECT, talk :: TALK | r)
  ~> Run (effect :: EFFECT | r)
evalTalk = Run.interpret (Run.on _talk handleTalk Run.send)

{- | Dinner DSL -}
data Food = Pizza | Chizburger
type IsThereMore = Boolean
type Bill = Int

data DinnerF next
  = Eat Food (IsThereMore -> next)
  | CheckPlease (Bill -> next)

derive instance functorDinnerF :: Functor DinnerF

type DINNER = FProxy DinnerF
_dinner = SProxy :: SProxy "dinner"

type Dinner a = Free DinnerF a

eat :: forall r. Food -> Run (dinner :: DINNER | r) IsThereMore
eat food = Run.lift _dinner $ Eat food identity

checkPlease :: forall r. Run (dinner :: DINNER | r) Bill
checkPlease = Run.lift _dinner $ CheckPlease identity

type State =
  { stock :: Int
  , bill :: Int
  }

handleDinner :: State -> DinnerF ~> Tuple State
handleDinner state@{ stock, bill } = case _ of
  Eat food reply ->
    let
      price :: Int
      price = case food of
        Pizza -> 10
        Chizburger -> 5
    in
      if stock >= price
      then
        Tuple { stock : stock - price, bill : bill + price } (reply true)
      else
        Tuple state (reply false)
  CheckPlease reply ->
    Tuple state (reply bill)

evalDinner :: forall r a. State -> Run (dinner :: DINNER | r) a -> Run r (Tuple Bill a)
evalDinner = Run.runAccumPure
  (\state -> Run.on _dinner (Loop <<< handleDinner state) Done)
  (\state next -> Tuple state.bill next)

{- | Coproduct Talk Dinner DSL -}

type LovelyEvening r = (talk :: TALK, dinner :: DINNER | r)

{- | Program -}

dinnerTime :: forall r. Run (LovelyEvening r) Unit
dinnerTime = do
  speak "Time to have some food."
  thereIsMore <- eat Pizza
  if thereIsMore
    then dinnerTime
    else do
      bill <- checkPlease
      speak "I'm done."

main :: Effect (Tuple Bill Unit)
main = dinnerTime
           # Run.runBaseEffect
         <<< evalDinner { stock : 20, bill : 0}
         <<< evalTalk

-- main :: Effect Unit
-- main = evalTalk "Jerry" echo
