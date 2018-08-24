{-# LANGUAGE
RankNTypes,
TupleSections
#-}

module Lens where

import Prelude
import Data.Tuple.Extra

type Lens s t a b = forall f.Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) =
  (,x) <$> f a

-- :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) =
  (x,) <$> f a

-- :: Fucntor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s ->f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s=
  (setter s) <$> f (getter s)


-- Lens (Either s1 s2) (Either t1 t2) a b
-- :: Functor f => (a -> f b) -> Either s1 s2 -> Either t1 t2
-- Lens s1 t1 a b :: Functor f => (a -> f b) -> s1 -> f t1
-- Lens s2 t2 a b :: Functor f => (a -> f b) -> s2 -> f t2
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 _ f (Left s1) = Left <$> l1 f s1
choosing _ l2 f (Right s2) = Right <$> l2 f s2

-- :: Functor f => (a -> f b) -> s -> f t
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(l <%~ f) s =
  l ((id &&& id) . f) s

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(l <<%~ f) s =
  l (id &&& f) s

-- Lens' s () :: (() -> f ()) -> s -> f s
united :: Lens' s ()
united f s = const s <$> f ()
