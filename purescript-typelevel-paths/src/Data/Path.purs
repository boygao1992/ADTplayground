module Data.Path where

import Prelude

import Data.Exists1 (Exists, mkExists)
import Data.Leibniz (type (~))

data Path i o
  = Path
  | PathId (i ~ o)
  | PathCompose (Exists (PathComposeF i o))
data PathComposeF i o x = PathComposeF (Path x o) (Path i x)
path = Path :: forall i o. Path i o
pathId = PathId identity :: forall i. Path i i
pathCompose :: forall i o x. Path x o -> Path i x -> Path i o
pathCompose xo ix = PathCompose $ mkExists $ PathComposeF xo ix

instance semigroupoidPath :: Semigroupoid Path where
  compose = pathCompose
instance categoryPath :: Category Path where
  identity = pathId
