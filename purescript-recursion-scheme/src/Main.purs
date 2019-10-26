module Main where

import Prelude

import Data.Newtype (class Newtype, un)
import Effect (Effect)
import Effect.Console (log)

------------
-- Recursive
------------

class Functor f <= Recursive t f | t → f where
  project :: t → f t

instance recursiveMu :: Functor f => Recursive (Mu f) f where
  project = unroll

instance recursiveListListF :: Recursive (List x) (ListF x) where
  project :: List x -> ListF x (List x)
  project =    -- ListF x (List x)
    map List   -- ListF x (Mu (ListF x))
    <<< project -- Mu (ListF x)
    <<< un List -- List x

--------------
-- Corecursive
--------------

class Functor f <= Corecursive t f | t → f where
  embed :: f t → t

instance corecursiveMu :: Functor f => Corecursive (Mu f) f where
  embed = roll
instance corecursiveListListF :: Corecursive (List x) (ListF x) where
  embed :: ListF x (List x) -> List x
  embed =            -- List x
    List             -- Mu (ListF x)
    <<< embed         -- ListF x (Mu (ListF x))
    <<< map (un List) -- ListF x (List x)

-----
-- Mu
-----

newtype Mu f = In (f (Mu f))
derive instance newtypeMu :: Newtype (Mu f) _

roll :: forall f. f (Mu f) -> Mu f
roll = In

unroll :: forall f. Mu f -> f (Mu f)
unroll (In x) = x

-------
-- List
-------

newtype List x = List (Mu (ListF x))
data ListF x a
  = Nil
  | Cons x a
derive instance newtypeList :: Newtype (List x) _
derive instance functorListF :: Functor (ListF x)

{- catamorphism-fusion law
h <<< f = g <<< map h
=>
h <<< cata f = cata g
where
  f :: f a -> a
  g :: f b -> b
  h :: a -> b

(h :: a -> b) <<< (f :: f a -> a) :: f a -> b
  =
(g :: f b -> b) <<< (map h :: f a -> f b) :: f a -> b

(h :: a -> b) <<< (cata f :: Fix f -> a) :: Fix f -> b
  =
cata g :: Fix f -> b
-}

{- catamorphism compose law (distributivity?)
cata f <<< cata (Fix <<< h) = cata (f <<< h)
where
  f :: f a -> a
  h :: g a -> f a
-}

{- banana-split theorem

cata f &&& cata g
  =
cata ((f <<< map fst) &&& (g <<< map snd))
-}

main :: Effect Unit
main = do
  log "🍝"
