module Data.Lens.Types where

import Data.Lens.Internal.Class.Wander (class Wander)
import Data.Lens.Internal.Forget (Forget)
import Data.Lens.Internal.Indexed (Indexed)
import Data.Lens.Internal.Shop (Shop)
import Data.Lens.Internal.Market (Market)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)

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

type Optic' p s a = Optic p s s a a

type Iso' s a = Iso s s a a

type Lens' s t a b = Lens s s a a

type Prism' s t a b = Prism s s a a

type Traversal' s a = Traversal s s a a

type Fold' r s a = Fold r s s a a

type Getter' s a = Getter s s a a

type Setter' s a = Setter s s a a

-- | Indexed Types

type IndexedOptic p i s t a b = Optic (Indexed p i) s t a b

type IndexedIso i s t a b = forall p. Profunctor p => IndexedOptic p i s t a b

type IndexedLens i s t a b = forall p. Strong p => IndexedOptic p i s t a b

type IndexedPrism i s t a b =  forall p. Choice p => IndexedOptic p i s t a b

type IndexedTraversal i s t a b = forall p. Wander p => IndexedOptic p i s t a b

type IndexedFold r i s t a b = IndexedOptic (Forget r) i s t a b

type IndexedGetter i s t a b = forall r. IndexedOptic (Forget r) i s t a b

type IndexedSetter i s t a b = IndexedOptic (->) i s t a b

-- | Indexed Simple Types

type IndexedOptic' p i s a = IndexedOptic p i s s a a

type IndexedIso' i s a = IndexedIso i s s a a

type IndexedLens' i s a = IndexedLens i s s a a

type IndexedPrism' i s a = IndexedPrism i s s a a

type IndexedTraversal' i s a = IndexedTraversal i s s a a

type IndexedFold' r i s a = IndexedFold r i s s a a

type IndexedGetter' i s a = IndexedGetter i s s a a

type IndexedSetter' i s a = IndexedSetter i s s a a

-- | Other

type ALens s t a b = Optic (Shop a b) s t a b

type APrism s t a b = Optic (Market a b) s t a b

type AGetter s t a b = Fold a s t a b
