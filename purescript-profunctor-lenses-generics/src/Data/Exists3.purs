module Data.Exists3 where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Lens (class Wander)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Exists3 :: ((Type -> Type -> Type) -> Type) -> Type
mkExists3 :: forall f p. f p -> Exists3 f
mkExists3 = unsafeCoerce
runExists3 :: forall f r. (forall p. f p -> r) -> Exists3 f -> r
runExists3 = unsafeCoerce

foreign import data CProfunctor :: ((Type -> Type -> Type) -> Type) -> Type
mkCProfunctor :: forall f p. Profunctor p => f p -> CProfunctor f
mkCProfunctor = unsafeCoerce
runCProfunctor :: forall f r. (forall p. Profunctor p => f p -> r) -> CProfunctor f -> r
runCProfunctor = unsafeCoerce

foreign import data CStrong :: ((Type -> Type -> Type) -> Type) -> Type
mkCStrong :: forall f p. Strong p => f p -> CStrong f
mkCStrong = unsafeCoerce
runCStrong :: forall f r. (forall p. Strong p => f p -> r) -> CStrong f -> r
runCStrong = unsafeCoerce

foreign import data CChoice :: ((Type -> Type -> Type) -> Type) -> Type
mkCChoice :: forall f p. Choice p => f p -> CChoice f
mkCChoice = unsafeCoerce
runCChoice :: forall f r. (forall p. Choice p => f p -> r) -> CChoice f -> r
runCChoice = unsafeCoerce

foreign import data CWander :: ((Type -> Type -> Type) -> Type) -> Type
mkCWander :: forall f p. Wander p => f p -> CWander f
mkCWander = unsafeCoerce
runCWander :: forall f r. (forall p. Wander p => f p -> r) -> CWander f -> r
runCWander = unsafeCoerce

-- Test
newtype OpticF s a p = OpticF (p a a -> p s s)
derive instance newtypeOpticF :: Newtype (OpticF s a p) _

newtype COptic' s a = COptic' (Exists3 (OpticF s a))
-- ~ forall p. p a a -> p s s
newtype CIso' s a = CIso' (CProfunctor (OpticF s a))
-- ~ forall p. Profunctor p => p a a -> p s s
newtype CLens' s a = CLens' (CStrong (OpticF s a))
-- ~ forall p. Strong p => p a a -> p s s
newtype CPrism' s a = CPrism' (CChoice (OpticF s a))
-- ~ forall p. Choice p => p a a -> p s s
newtype CTraversal' s a = CTraversal' (CWander (OpticF s a))
-- ~ forall p. Wander p => p a a -> p s s

runCTraversal'
  :: forall s a r. (forall p. Wander p => (p a a -> p s s) -> r) -> CTraversal' s a -> r
runCTraversal' f (CTraversal' cwander) = runCWander (f <<< unwrap) cwander
