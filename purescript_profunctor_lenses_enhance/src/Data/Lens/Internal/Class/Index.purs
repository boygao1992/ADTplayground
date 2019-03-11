module Data.Lens.Internal.Class.Index where

import Data.Lens.Types (Traversal')

class Index m i a | m -> i, m -> a where
  -- forall p. Wander p => p (Tuple i a) b -> p (Tuple i s) t
  ix :: i -> Traversal' m a
