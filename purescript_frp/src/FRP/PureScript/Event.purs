module FRP.PureScript.Event where

import Prelude

import Data.Traversable (traverse_)
import Unsafe.Reference (unsafeRefEq)
import Data.Array as Array
import Effect.Ref as Ref
import Data.Compactable (class Compactable)
import Data.Either
import Data.Maybe
import Data.Newtype (class Newtype)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)


newtype Event a = Event ((a -> Effect Unit) -> Effect (Effect Unit))
derive instance newtypeEvent :: Newtype (Event a) _

instance functorEvent :: Functor Event where
  map :: forall a b. (a -> b) -> Event a -> Event b
  map f (Event e) = Event \k -> e (k <<< f)

-- Constructor
create
  :: forall a
   . Effect
       { event :: Event a
       , push :: a -> Effect Unit
       }
create = do
  subscribers <- Ref.new []
  let
    event :: (a -> Effect Unit) -> Effect (Effect Unit)
    event k = do
      Ref.modify_ (Array.snoc <@> k) subscribers
      pure $ Ref.modify_ (Array.deleteBy unsafeRefEq k) subscribers
    push a = Ref.read subscribers >>= traverse_ \k -> k a
  pure
    { event : Event event
    , push
    }

-- Operators
subscribe
  :: forall a
   . Event a
  -> (a -> Effect Unit)
  -> Effect (Effect Unit)
subscribe (Event e) k = e k

filter :: forall a. (a -> Boolean) -> Event a -> Event a
filter pred (Event e) = Event \k -> e (switch k)
  where
    switch :: (a -> Effect Unit) -> a -> Effect Unit
    switch k a =
      if pred a
      then k a
      else pure unit

foldr :: forall a b. (a -> b -> b) -> Event a -> b -> Event b
foldr reducer (Event e) init = Event \k -> do
  acc <- Ref.new init
  e \a -> Ref.modify (reducer a) acc >>= k


-- Continuation-Passing-Style Attempt

-- continuation-passing-style definition of Event (not exact)
-- newtype ContT r m a = ContT ((a -> m r) -> m r)
-- newtype Event a = Event (ContT Unit Effect a)
-- derive instance newtypeEvent :: Newtype (Event a) _
-- derive newtype instance functorEvent :: Functor Event

-- filter :: forall a. (a -> Boolean) -> Event a -> Event a
-- filter pred = over Event (withContT filterContT)
--   where
--     filterContT :: (a -> Effect Unit) -> a -> Effect Unit
--     filterContT k a =
--       if pred a
--       then k a
--       else pure unit

-- -- use a mutable object to hold the long-living state of fold
-- fold :: forall a b. (a -> b -> b) -> Event a -> b -> Event b
-- fold f event b = over Event (withContT foldContT)
--   where
--     foldContT :: (a -> Effect Unit) -> b -> Effect Unit
--     foldContT k initialState = do
--       state <- Ref.new initialState
