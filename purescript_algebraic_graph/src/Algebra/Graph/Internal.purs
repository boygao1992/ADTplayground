module Algebra.Graph.Internal where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (foldr)

maybeF :: forall a b. (a -> b -> a) -> a -> Maybe b -> Maybe a
maybeF f x = pure <<< maybe x (f x)

foldr1Safe :: forall a. (a -> a -> a) -> Array a -> Maybe a
foldr1Safe f = foldr (maybeF f) Nothing
