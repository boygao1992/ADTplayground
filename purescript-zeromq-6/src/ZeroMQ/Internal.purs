module ZeroMQ.Internal
  ( Address(..)
  , bind
  , connect
  , disconnect
  , sendMany
  , unbind
  ) where

import Prelude
import Control.Promise (Promise)
import Control.Promise as Control.Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (Buffer)
import ZeroMQ.Types (Socket)

newtype Address
  = Address String

bind ::
  Socket ->
  Address ->
  Aff Unit
bind socket (Address address) =
  Control.Promise.toAff
    $ unsafePerformEffect
    $ runEffectFn2 bindImpl socket address

foreign import bindImpl :: EffectFn2 Socket String (Promise Unit)

connect :: Socket -> Address -> Effect Unit
connect socket (Address address) = runEffectFn2 connectImpl socket address

foreign import connectImpl :: EffectFn2 Socket String Unit

disconnect :: Socket -> Address -> Effect Unit
disconnect socket (Address address) = runEffectFn2 disconnectImpl socket address

foreign import disconnectImpl :: EffectFn2 Socket String Unit

sendMany :: Socket -> Array Buffer -> Aff Unit
sendMany socket message =
  Control.Promise.toAff
    $ unsafePerformEffect
    $ runEffectFn2 sendManyImpl socket message

foreign import sendManyImpl :: EffectFn2 Socket (Array Buffer) (Promise Unit)

unbind :: Socket -> Address -> Aff Unit
unbind socket (Address address) =
  Control.Promise.toAff
    $ unsafePerformEffect
    $ runEffectFn2 unbindImpl socket address

foreign import unbindImpl :: EffectFn2 Socket String (Promise Unit)
