module AVar where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (Error)

foreign import data AVar :: Type -> Type

type Canceller = Effect Unit

type AVarCallback a = Either Error a -> Effect Unit

data AVarStatus a
  = Killed Error
  | Filled a
  | Empty

-- | Foreign Function Interface Utility
type FFIUtil =
  { left :: forall a b. a -> Either a b
  , right :: forall a b. b -> Either a b
  , nothing :: forall a. Maybe a
  , just :: forall a. a -> Maybe a
  , killed :: forall a. Error -> AVarStatus a
  , filled :: forall a. a -> AVarStatus a
  , empty :: forall a. AVarStatus a
  }

ffiUtil :: FFIUtil
ffiUtil =
  { left : Left
  , right : Right
  , nothing : Nothing
  , just : Just
  , killed : Killed
  , filled : Filled
  , empty : Empty
  }

-- | Constructors
foreign import empty :: forall a. Effect (AVar a)

foreign import _newVar :: forall a. a -> Effect (AVar a)
new :: forall a. a -> Effect (AVar a)
new = _newVar

-- | Commands
foreign import _killVar :: forall a. Fn.Fn3 FFIUtil Error (AVar a)  (Effect Unit)
kill :: forall a. Error -> (AVar a) -> Effect Unit
kill = Fn.runFn3 _killVar ffiUtil

foreign import _putVar :: forall a. Fn.Fn4 FFIUtil a (AVar a) (AVarCallback a) (Effect Canceller)
put :: forall a. a -> AVar a -> AVarCallback a -> Effect Canceller
put = Fn.runFn4 _putVar ffiUtil

foreign import _takeVar :: forall a. Fn.Fn3 FFIUtil (AVar a) (AVarCallback a) (Effect Canceller)
take :: forall a. AVar a -> AVarCallback a -> Effect Canceller
take = Fn.runFn3 _takeVar ffiUtil

foreign import _readVar :: forall a. Fn.Fn3 FFIUtil (AVar a) (AVarCallback a) (Effect Canceller)
read :: forall a. AVar a -> AVarCallback a -> Effect Canceller
read = Fn.runFn3 _readVar ffiUtil

foreign import _tryPutVar :: forall a. Fn.Fn3 FFIUtil a (AVar a) (Effect Boolean)
tryPut :: forall a. a -> AVar a -> Effect Boolean
tryPut = Fn.runFn3 _tryPutVar ffiUtil

foreign import _tryTakeVar :: forall a. Fn.Fn2 FFIUtil (AVar a) (Effect (Maybe a))
tryTake :: forall a. AVar a -> Effect (Maybe a)
tryTake = Fn.runFn2 _tryTakeVar ffiUtil

foreign import _tryReadVar :: forall a. Fn.Fn2 FFIUtil (AVar a) (Effect (Maybe a))
tryRead :: forall a. AVar a -> Effect (Maybe a)
tryRead = Fn.runFn2 _tryReadVar ffiUtil

-- | Predicates
foreign import _status :: forall a. Fn.Fn2 FFIUtil (AVar a) (Effect (AVarStatus a))
status :: forall a. AVar a -> Effect (AVarStatus a)
status = Fn.runFn2 _status ffiUtil

isKilled :: forall a. AVarStatus a -> Boolean
isKilled (Killed _) = true
isKilled _ = false

isFilled :: forall a. AVarStatus a -> Boolean
isFilled (Filled _) = true
isFilled _ = false

isEmpty :: forall a. AVarStatus a -> Boolean
isEmpty Empty = true
isEmpty _ = false
