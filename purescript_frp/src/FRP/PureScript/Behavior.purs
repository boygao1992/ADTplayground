module FRP.PureScript.Behavior where

import Prelude

import FRP.PureScript.Event (Event)

newtype ABehavior (event :: Type -> Type) i =
  ABehavior (forall o. event (i -> o) -> event o)

type Behavior = ABehavior Event

instance functorABehavior :: Functor event => Functor (ABehavior event) where
  map :: forall a b. (a -> b) -> ABehavior event a -> ABehavior event b
  map f (ABehavior actuate) = ABehavior \sampler -> actuate (map (_ <<< f) sampler)

-- | Constructor
behavior :: forall event i. (forall o. event (i -> o) -> event o) -> ABehavior event i
behavior = ABehavior

-- | Operators
