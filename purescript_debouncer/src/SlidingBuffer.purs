module SlidingBuffer where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))

-- | Types
type SlidingBuffer a =
  { buffer :: Array a
  , size :: Int
  }

-- | Constructors
singleton :: forall a. a -> Int -> SlidingBuffer a
singleton x size = { buffer : A.singleton x, size } -- TODO size >= 1

fromArray :: forall a. Array a -> Int -> SlidingBuffer a
fromArray xs size = { buffer : xs, size } -- TODO size >= 1

-- | Getters
getBuffer :: forall a. SlidingBuffer a -> Array a
getBuffer = _.buffer

getSize :: forall a. SlidingBuffer a -> Int
getSize = _.size

-- | Operators
putLeft :: forall a. a -> SlidingBuffer a -> SlidingBuffer a -- slide to the left
putLeft x sb@{ buffer, size }
  = sb { buffer = A.dropEnd (A.length buffer + 1 - size) <<< A.cons x $ buffer }

putRight :: forall a. a -> SlidingBuffer a -> SlidingBuffer a -- slide to the left
putRight x sb@{ buffer, size }
  = sb { buffer = A.drop (A.length buffer + 1 - size) <<< flap(A.snoc) x $ buffer }

head :: forall a. SlidingBuffer a -> Maybe a
head { buffer } = A.head buffer

last :: forall a. SlidingBuffer a -> Maybe a
last { buffer } = A.last buffer

empty :: forall a. SlidingBuffer a -> SlidingBuffer a
empty = _ { buffer = [] }
