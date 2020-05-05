module Streamly.Internal where

import Data.Maybe (Maybe)

data SVar (t :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) a

data SVarStyle
  = AsyncVar -- depth first concurrent
  | WAsyncVar -- breadth first concurrent
  | ParallelVar -- all parallel
  | AheadVar -- concurrent look ahead

type State t m a
  = { streamVar :: Maybe (SVar t m a)
    , _yieldLimit :: Maybe Count
    , _threadsHigh :: Limit
    , _bufferHigh :: Limit
    , _streamLatency :: Maybe NanoSecond64
    , _maxStreamRate :: Maybe Rate
    , _inspectMode :: Boolean
    }

newtype Count
  = Count Int -- Int64

data Limit
  = Unlimited
  | Limited

newtype NanoSecond64
  = NanoSecond64 Int -- Int64

type Rate
  = { low :: Number
    , goal :: Number
    , high :: Number
    , buffer :: Int
    }

newtype Stream m a
  = Stream
  ( forall r.
    State Stream m a -> -- state
    (a -> Stream m a -> m r) -> -- yield
    (a -> m r) -> -- singleton
    m r -> -- stop
    m r
  )
