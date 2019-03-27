module Algebra.Graph where

import Prelude

import Algebra.Graph.Internal (foldr1Safe)
import Data.Maybe (fromMaybe)


data Graph a
  = Empty
  | Vertex a
  | Overlay (Graph a) (Graph a)
  | Connect (Graph a) (Graph a)


empty :: forall a. Graph a
empty = Empty

vertex :: forall a. a -> Graph a
vertex = Vertex

overlay :: forall a. Graph a -> Graph a -> Graph a
overlay = Overlay

connect :: forall a. Graph a -> Graph a -> Graph a
connect = Connect

edge :: forall a. a -> a -> Graph a
edge x y = connect (vertex x) (vertex y)

overlays :: forall a. Array (Graph a) -> Graph a
overlays = fromMaybe empty <<< foldr1Safe overlay
