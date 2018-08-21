module GraphTest where

import Prelude
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)

data EdgeSpec = EdgeSpec
  { _fromNode :: Int
  , _toNode :: Int
  , _distance :: Int
  }

newtype NodeLabel = NodeLabel Int
type Distance = Int

genGraph :: (Int, [EdgeSpec]) -> Gr NodeLabel Distance
genGraph (numNodes, edgeSpecs) = mkGraph nodes edges
  where
      nodes = (\i -> (i, NodeLabel i)) <$> [1..numNodes]
      edges = (\es -> (_fromNode es, _toNode es, _distance es)) <$> edgeSpecs

