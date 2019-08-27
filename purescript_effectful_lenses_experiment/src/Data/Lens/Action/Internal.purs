module Data.Lens.Action.Internal where

import Prelude

import Data.Identity
import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice
import Data.Profunctor.Cochoice
import Data.Newtype
import Data.Lens

type Action m s a = forall r p. Effective m r p => Optic' p s a
type IndexedAction i m s a = forall r p. Effective m r p => IndexedOptic' p i s a

type MonadFold m s a = forall r p. Effective m r p => Wander p => Optic' p s a
type IndexedMonadFold i m s a = forall r p. Effective m r p => Wander p => IndexedOptic' p i s a

class Effective m r p | p -> m r where
  effective :: forall a. (a -> m r) -> p a a
  ineffective :: forall a. p a a -> a -> m r

instance effectiveForget :: Effective Identity r (Forget r) where
  effective aIr = Forget (unwrap <<< aIr)
  ineffective (Forget aMr) = Identity <<< aMr

newtype EffectM m r a b = EffectM (Forget (m r) a b)
instance newtypeEffectM :: Newtype (EffectM m r a b) (a -> m r) where
  wrap = EffectM <<< Forget
  unwrap (EffectM (Forget x)) = x
derive newtype instance semigroupEffectM ::
  Semigroup (m r) => Semigroup (EffectM m r a b)
derive newtype instance monoidEffectM :: Monoid (m r) => Monoid (EffectM m r a b)
derive newtype instance profunctorEffectM :: Profunctor (EffectM m r)
derive newtype instance choiceEffectM :: Monoid (m r) => Choice (EffectM m r)
derive newtype instance strongEffectM :: Strong (EffectM m r)
derive newtype instance cochoiceEffectM :: Cochoice (EffectM m r)
derive newtype instance wanderEffectM :: Monoid (m r) => Wander (EffectM m r)

-- (a -> m r) -> (s -> m r)
type Acting m r s a = Optic' (EffectM m r) s a

perform :: forall m s a. Monad m => Acting m a s a -> s -> m a
perform l = unwrap $ l $ wrap pure

performOn :: forall m s a. Monad m => s -> Acting m a s a -> m a
performOn = flip perform
infixl 8 performOn as ^!
