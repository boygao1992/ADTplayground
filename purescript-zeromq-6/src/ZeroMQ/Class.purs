module ZeroMQ.Class where

import ZeroMQ.Types

{- [Interface Readable<M>](http://zeromq.github.io/zeromq.js/interfaces/readable.html)
  M: object[]
    The type of the message or message parts that can be read.

* Method
  - receive(): Promise<M>

[source code](https://github.com/zeromq/zeromq.js/blob/ed54323ef8a7b1b330ae35ad51fa9a3438bf35d2/src/index.ts#L180)
-}
class Readable (a :: SocketType)

instance readablePull :: Readable Pull

instance readableSubscriber :: Readable Subscriber

{- [Interface Writable<M, O>](http://zeromq.github.io/zeromq.js/interfaces/writable.html)
  M: MessageLike | MessageLike[]
    The type of the message or message parts that can be sent.
  O: [Array]
    Rest type for any options, if applicable to the socket type (DRAFT only).

* Method
  - send(message: M, ...options: O): Promise<void>

[source code](https://github.com/zeromq/zeromq.js/blob/ed54323ef8a7b1b330ae35ad51fa9a3438bf35d2/src/index.ts#L57)
-}
class Writable (a :: SocketType)

instance writablePublisher :: Writable Publisher

instance writablePush :: Writable Push

class Bindable (from :: SocketType)

instance bindablePublisher :: Bindable Publisher

instance bindablePull :: Bindable Pull

instance bindablePush :: Bindable Push

class Connectable (from :: SocketType)

instance connectablePublisher :: Connectable Publisher

instance connectableSubscriber :: Connectable Subscriber

instance connectablePull :: Connectable Pull

instance connectablePush :: Connectable Push
