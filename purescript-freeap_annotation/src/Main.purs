module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Control.Monad.Free (Free, liftF)

data PlanAF o a
  = PlanA_Idle a
  | Yield o a

data PlanBF i a
  = PlanB_Idle a
  | Emit (i -> a)

newtype PlanF i o a = PlanF (PlanAF o (PlanBF i a))

newtype Plan i o a = Plan (Free (PlanF i o) a)

sampleYield :: forall i o. o -> PlanAF o (PlanBF i Unit)
sampleYield o = Yield o $ PlanB_Idle unit

sampleEmit :: forall i o. PlanAF o (PlanBF i i)
sampleEmit = PlanA_Idle $ Emit identity

yield :: forall i o. o -> Plan i o Unit
yield o = Plan $ liftF $ PlanF $ Yield o $ PlanB_Idle unit

emit :: forall i o. Plan i o i
emit = Plan $ liftF $ PlanF $ PlanA_Idle $ Emit identity

main :: Effect Unit
main = do
  log "Hello sailor!"
