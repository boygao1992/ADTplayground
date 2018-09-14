module FreeT where

import Prelude

import Data.Either (Either(..))
import Data.Exists (Exists(..), runExists)
import Data.Functor (class Functor)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)

data FreeT f m a
  = FreeT (Unit -> m (Either a (f (FreeT f m a))))
  | Gosub (Exists (GosubF f m a))

data GosubF f m b a = GosubF (Unit -> FreeT f m a) (a -> FreeT f m b)

