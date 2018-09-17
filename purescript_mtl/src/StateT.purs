module State.Trans where

import Prelude
import Data.Tuple (Tuple(..))

newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT s) = s


