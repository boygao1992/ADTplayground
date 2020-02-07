module ZeroMQ.Types
  ( Socket
  , kind SocketType
  , Publisher
  , Subscriber
  , Push
  , Pull
  ) where

{- [Socket](http://zeromq.github.io/zeromq.js/classes/socket.html)

* Property
  - closed: boolean

* Method
  - bind(address: string): Promise<void>
  - close(): void
  - connect(address: string): void
  - disconnect(address: string): void
  - unbind(address: string): Promise<void>

-}
foreign import data Socket :: SocketType -> Type

foreign import kind SocketType

foreign import data Publisher :: SocketType

{- [(Socket, Readable<M>) <= Subscriber<M>](http://zeromq.github.io/zeromq.js/classes/subscriber.html)
  M: object[]

* Method
  - subscribe(...prefixes: Array<Buffer | string>): void
-}
foreign import data Subscriber :: SocketType

foreign import data Push :: SocketType

foreign import data Pull :: SocketType
