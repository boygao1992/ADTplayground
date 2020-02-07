module ZeroMQ.Internal
  ( placeholder -- TODO remove when there's something to export
  ) where

import Prelude
import Control.Promise (Promise)
import Control.Promise as Control.Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (Buffer)
import ZeroMQ.Types (Bound, Connected, Socket)

placeholder = "" :: String -- TODO remove when there's something to export

newtype Address
  = Address String

bind ::
  forall from to.
  Socket from to Bound ->
  Address ->
  Effect Unit
bind socket (Address address) = runEffectFn2 bindImpl socket address

foreign import bindImpl ::
  forall from to.
  EffectFn2
    (Socket from to Bound)
    String
    Unit

connect ::
  forall from to.
  Socket from to Connected ->
  Address ->
  Effect Unit
connect socket (Address address) = runEffectFn2 connectImpl socket address

foreign import connectImpl ::
  forall from to.
  EffectFn2
    (Socket from to Connected)
    String
    Unit

sendMany ::
  forall from to loc.
  Socket from to loc ->
  Array Buffer ->
  Aff Unit
sendMany socket message =
  Control.Promise.toAff
    $ unsafePerformEffect
    $ runEffectFn2 sendManyImpl socket message

foreign import sendManyImpl ::
  forall from to loc.
  EffectFn2
    (Socket from to loc)
    (Array Buffer)
    (Promise Unit)
