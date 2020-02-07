module ZeroMQ where

import Prelude
import Control.Promise (Promise)
import Control.Promise as Control.Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (Buffer)

foreign import kind Location

foreign import data Bound :: Location

foreign import data Connected :: Location

foreign import kind SocketType

foreign import data Pub :: SocketType

foreign import data Sub :: SocketType

foreign import data Push :: SocketType

foreign import data Pull :: SocketType

foreign import data Socket :: SocketType -> SocketType -> Location -> Type

{- [Interface Readable<M>](http://zeromq.github.io/zeromq.js/interfaces/readable.html)
  M: object[]
    The type of the message or message parts that can be read.

[source code](https://github.com/zeromq/zeromq.js/blob/ed54323ef8a7b1b330ae35ad51fa9a3438bf35d2/src/index.ts#L180)
-}
class Readable (a :: SocketType)

{- [Interface Writable<M, O>](http://zeromq.github.io/zeromq.js/interfaces/writable.html)
  M: MessageLike | MessageLike[]
    The type of the message or message parts that can be sent.
  O: [Array]
    Rest type for any options, if applicable to the socket type (DRAFT only).

[source code](https://github.com/zeromq/zeromq.js/blob/ed54323ef8a7b1b330ae35ad51fa9a3438bf35d2/src/index.ts#L57)
-}
class Writable (a :: SocketType)

connect ::
  forall from to.
  Socket from to Connected ->
  String ->
  Effect Unit
connect = runEffectFn2 connectImpl

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
