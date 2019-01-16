module Control.Monad.ST.Unsafe where

import Control.Monad.ST.Ref (STRef)

foreign import unsafeReadSTRef
  :: forall h a
     . STRef h a -> a
