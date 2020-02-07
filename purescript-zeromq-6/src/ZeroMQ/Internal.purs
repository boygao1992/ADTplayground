module ZeroMQ.Internal
  ( Address(..)
  , bind
  , closed
  , connect
  , disconnect
  , newPublisher
  , newPull
  , newPush
  , newSubscriber
  , receive
  , sendMany
  , subscribe
  , subscribeAll
  , unbind
  ) where

import Prelude
import Control.Promise (Promise)
import Control.Promise as Control.Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Node.Buffer.Immutable (ImmutableBuffer)
import ZeroMQ.Types (Publisher, Pull, Push, Socket, Subscriber)

newtype Address
  = Address String

bind :: forall from. Socket from -> Address -> Aff Unit
bind socket (Address address) =
  Control.Promise.toAffE
    $ runEffectFn2 bindImpl socket address

foreign import bindImpl :: forall from. EffectFn2 (Socket from) String (Promise Unit)

closed :: forall from. Socket from -> Effect Boolean
closed = runEffectFn1 closedImpl

foreign import closedImpl :: forall from. EffectFn1 (Socket from) Boolean

connect :: forall from. Socket from -> Address -> Effect Unit
connect socket (Address address) = runEffectFn2 connectImpl socket address

foreign import connectImpl :: forall from. EffectFn2 (Socket from) String Unit

disconnect :: forall from. Socket from -> Address -> Effect Unit
disconnect socket (Address address) = runEffectFn2 disconnectImpl socket address

foreign import disconnectImpl :: forall from. EffectFn2 (Socket from) String Unit

foreign import newPublisher :: Effect (Socket Publisher)

foreign import newPull :: Effect (Socket Pull)

foreign import newPush :: Effect (Socket Push)

foreign import newSubscriber :: Effect (Socket Subscriber)

receive :: forall from. Socket from -> Aff (Array ImmutableBuffer)
receive socket =
  Control.Promise.toAffE
    $ runEffectFn1 receiveImpl socket

foreign import receiveImpl :: forall from. EffectFn1 (Socket from) (Promise (Array ImmutableBuffer))

sendMany :: forall from. Socket from -> Array ImmutableBuffer -> Aff Unit
sendMany socket message =
  Control.Promise.toAffE
    $ runEffectFn2 sendManyImpl socket message

foreign import sendManyImpl :: forall from. EffectFn2 (Socket from) (Array ImmutableBuffer) (Promise Unit)

subscribe :: forall from. Socket from -> Array ImmutableBuffer -> Effect Unit
subscribe = runEffectFn2 subscribeImpl

foreign import subscribeImpl :: forall from. EffectFn2 (Socket from) (Array ImmutableBuffer) Unit

subscribeAll :: forall from. Socket from -> Effect Unit
subscribeAll = runEffectFn1 subscribeAllImpl

foreign import subscribeAllImpl :: forall from. EffectFn1 (Socket from) Unit

unbind :: forall from. Socket from -> Address -> Aff Unit
unbind socket (Address address) =
  Control.Promise.toAffE
    $ runEffectFn2 unbindImpl socket address

foreign import unbindImpl :: forall from. EffectFn2 (Socket from) String (Promise Unit)
