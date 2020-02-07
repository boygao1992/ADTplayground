module ZeroMQ.Types
  ( Socket
  , kind SocketType
  , Publisher
  , Subscriber
  , Push
  , Pull
  , kind Location
  , Bound
  , Connected
  ) where

{- [Class Socket](http://zeromq.github.io/zeromq.js/classes/socket.html)

* Property
  - closed: boolean

* Method
  - bind(address: string): Promise<void>
  - close(): void
  - connect(address: string): void
  - disconnect(address: string): void
  - unbind(address: string): Promise<void>

-}
foreign import data Socket :: SocketType -> SocketType -> Location -> Type

foreign import kind SocketType

foreign import data Publisher :: SocketType

foreign import data Subscriber :: SocketType

foreign import data Push :: SocketType

foreign import data Pull :: SocketType

foreign import kind Location

foreign import data Bound :: Location

foreign import data Connected :: Location
