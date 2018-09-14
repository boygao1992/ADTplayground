module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref

main :: Effect Unit
main = do
  -- Ref.new :: forall s. s -> Effect (Ref s)
  ref <- Ref.new 0 -- Effect (Ref Int)

  -- Ref.read :: forall s. Ref s -> Effect s
  state0 <- Ref.read ref

  log $ "state0 = " <> show state0

  -- Ref.write :: forall s. s -> Ref s -> Effect Unit
  Ref.write 1 ref

  state1 <- Ref.read ref

  log $ "state1 = " <> show state1

  -- Ref.modify :: forall s. (s -> s) -> Ref s -> Effect s
  state2 <- Ref.modify (\x -> x + 10) ref

  log $ "state2 = " <> show state2

  -- Ref.modify' :: forall s b. (s -> { state :: s, value :: b }) -> Ref s -> Effect b
  val <- Ref.modify' (\x -> { state : x + 20, value : x * 2 } ) ref

  log $ "val = " <> show val

  state3 <- Ref.read ref

  log $ "state3 = " <> show state3


