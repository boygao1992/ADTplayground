module Data.Lens.Internal.Indexed where

import Prelude

import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (second)
import Data.Tuple (Tuple)

newtype Indexed p i a b = Indexed (p (Tuple i a) b)
instance profunctorIndexed :: Profunctor p => Profunctor (Indexed p i) where
  dimap
    :: forall s t a b
       . (s -> a)
    -> (b -> t)
    -> Indexed p i a b -- p (Tuple i a) b
    -> Indexed p i s t -- p (Tuple i s) t
  dimap pre post (Indexed p) = Indexed
                               <<< dimap -- :: (Tuple i s -> Tuple i a) -> (b -> t) -> p (Tuple i a) b -> p (Tuple i s) t
                               (second pre) -- :: forall i. Tuple i s -> Tuple i a
                               post -- :: b -> t
                               $ p
