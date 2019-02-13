module FRP.Classic where

import Prelude
import Data.Tuple (Tuple (..))
import Data.Array (uncons)
import Data.Maybe (Maybe (..))

-- 2.1.1 History of Classic FRP
-- Figure 2.2

type Time = Number

newtype Event a = Event (Array (Tuple Time a))
instance functorEvent :: Functor Event where
  map f (Event arr) = Event $ map (\(Tuple t a) -> Tuple t (f a)) arr

newtype Behavior a = Behavior (Time -> a)
derive newtype instance functorBehavior :: Functor Behavior
instance applyBehavior :: Apply Behavior where
  apply (Behavior bf) (Behavior ba) = Behavior $ \t ->
    bf t (ba t)

time :: Behavior Time
time = Behavior identity

constant :: forall a. a -> Behavior a
constant = Behavior <<< const

delayB :: forall a. a -> Time -> Behavior a -> Behavior a
delayB a untilT (Behavior b) = Behavior $ \currentTime ->
  if currentTime <= untilT
  then a
  else b (currentTime - untilT)

never :: forall a. Event a
never = Event []

once :: forall a. a -> Event a
once a = Event [Tuple 0.0 a]

delayE :: forall a. Time -> Event a -> Event a
delayE deltaT (Event arr)= Event $ map (\(Tuple t a) -> Tuple (t + deltaT) a) arr

-- NOTE original implementation is wrong
switcher :: forall a. Behavior a -> Event (Behavior a) -> Behavior a
switcher (Behavior f) (Event arr) = Behavior $ \t -> case uncons arr of
  Nothing -> f t
  Just { head: Tuple t0 b0, tail } ->
    if t < t0
    then f t
    else go b0 tail t
  where
    go :: Behavior a -> Array (Tuple Time (Behavior a)) -> Time -> a
    go (Behavior f0) tail t = case uncons tail of
      Nothing -> f0 t
      Just { head: Tuple t1 b1, tail: t_tail } ->
        if t < t1
        then f0 t
        else go b1 t_tail t
