module Data.Lens.Internal.Class.Wander where

import Prelude

import Data.Profunctor.Strong (class Strong)
import Data.Profunctor.Choice (class Choice)

class (Strong p, Choice p) <= Wander p where
  wander
    :: forall s t a b
                                              -- TODO currying
     . (forall f. Applicative f => (a -> f b) -> (s -> f t))
              -- TODO currying
    -> (p a b -> p s t)
