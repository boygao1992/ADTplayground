module Reactive.Banana.Prim.Graph
  ( Graph
  , GraphM
  , dfs
  , emptyGraph
  , insertEdge
  ) where

import Prelude

import Data.Array as Data.Array
import Data.HashMap as Data.HashMap
import Data.HashSet as Data.HashSet
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))

type Graph a =
  { children :: Data.HashMap.HashMap a (Array a)
  , parents :: Data.HashMap.HashMap a (Array a)
  , nodes :: Data.HashSet.HashSet a
  }

emptyGraph :: forall a. Graph a
emptyGraph =
  { children: Data.HashMap.empty
  , parents: Data.HashMap.empty
  , nodes: Data.HashSet.empty
  }

insertEdge :: forall a.
  Hashable a =>
  { from :: a, to :: a } ->
  Graph a ->
  Graph a
insertEdge { from, to } graph =
  graph
    { children = Data.HashMap.insertWith (flip (<>)) from [to] graph.children
    , parents = Data.HashMap.insertWith (flip (<>)) to [from] graph.parents
    , nodes = Data.HashSet.insert from <<< Data.HashSet.insert to $ graph.nodes
    }

type GraphM m a = a -> m (Array a)

dfs ::
  forall a m.
  Hashable a =>
  Monad m =>
  GraphM m a ->
  a ->
  m (Array a)
dfs succ x = map _.result $ go [x] [] Data.HashSet.empty
  where
  go ::
    Array a ->
    Array a ->
    Data.HashSet.HashSet a ->
    m { result :: Array a, traversed :: Data.HashSet.HashSet a }
  go xs ys traversed = case Data.Array.uncons xs of
    Nothing -> pure { result: ys, traversed }
    Just { head, tail }
      | head `Data.HashSet.member` traversed -> go tail ys traversed
      | otherwise -> do
          xs' <- succ head
          ({result: ys', traversed: traversed' }) <- go xs' ys (Data.HashSet.insert head traversed)
          go tail (head `Data.Array.cons` ys') traversed'
