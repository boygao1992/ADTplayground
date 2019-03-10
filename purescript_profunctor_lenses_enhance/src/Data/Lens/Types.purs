module Data.Lens.Types where

import Prelude

import Data.Tuple (Tuple)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (class Strong, second)
import Data.Profunctor.Choice (class Choice)

class (Strong p, Choice p) <= Wander p where
  wander
    :: forall s t a b
                                              -- TODO currying
     . (forall f. Applicative f => (a -> f b) -> (s -> f t))
              -- TODO currying
    -> (p a b -> p s t)

newtype Forget r a b = Forget (a -> r)
instance profunctorForget :: Profunctor (Forget r) where
  dimap :: forall s t a b. (s -> a) -> (b -> t) -> Forget r a b -> Forget r s t
  -- dimap pre post (Forget p) = Forget $ dimap pre identity p
  dimap pre post (Forget f) = Forget (pre >>> f)

newtype Indexed p i a b = Indexed (p (Tuple i a) b)
instance profunctorIndexed :: Profunctor p => Profunctor (Indexed p i) where
  dimap
    :: forall s t a b
     . (s -> a)
    -> (b -> t)
    -> Indexed p i a b -- p (Tuple i a) b
    -> Indexed p i s t -- p (Tuple i s) t
  dimap pre post (Indexed p) = Indexed
    <<< dimap -- :: (Tuple i s -> Tuple i a) -> (b -> t) -> p (Tuple i a) b -> p (Tuple i s) t
          (second pre) -- :: forall i. Tuple i s -> Tuple i a
          post -- :: b -> t
      $ p

class Index m i a | m -> i, m -> a where
  -- forall p. Wander p => p (Tuple i a) b -> p (Tuple i s) t
  ix :: i -> Traversal' m a

-- | Types

type Optic p s t a b = p a b -> p s t

type Iso s t a b = forall p. Profunctor p => Optic p s t a b

type Lens s t a b = forall p. Strong p => Optic p s t a b

type Prism s t a b = forall p. Choice p => Optic p s t a b

type Traversal s t a b = forall p. Wander p => Optic p s t a b

type Fold r s t a b = Optic (Forget r) s t a b

type Getter s t a b = forall r. Fold r s t a b

type Setter s t a b = Optic (->) s t a b

-- | Simple Types (no structural transformation)

type Traversal' s a = Traversal s s a a

-- | Indexed Types
type IndexedOptic p i s t a b = Optic (Indexed p i) s t a b

type IndexedIso i s t a b = forall p. Profunctor p => IndexedOptic p i s t a b

type IndexedLens i s t a b = forall p. Strong p => IndexedOptic p i s t a b

type IndexedPrism i s t a b =  forall p. Choice p => IndexedOptic p i s t a b

type IndexedFold r i s t a b = IndexedOptic (Forget r) i s t a b

type IndexedGetter i s t a b = forall r. IndexedOptic (Forget r) i s t a b

type IndexedSetter i s t a b = IndexedOptic (->) i s t a b

type IndexedTraversal i s t a b = forall p. Wander p => IndexedOptic p i s t a b



-- | Indexed Simple Types
