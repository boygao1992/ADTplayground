module Control.Selective where

import Prelude

import Data.Const (Const(..))
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe, maybe)

type Selector f a b = f (a \/ b)

{- laws
1. identity:
  - x <*? pure identity = either identity identity <$> x
2. distributivity over (<*):
  - pure x <*? (y <* z) = (pure x <*? y) <* (pure x <*? z)
3. associativity
  - x <*? (y <*? z) = (f <$> x) <*? (g <$> y) <*? (h <$> z)
  where
    f x = Right <$> x
    g y = a -> bimap (,a) ($a) y
    h z = uncurry z
4. generalized identity:
  - x <*? pure f = either f identity <$> x
-}
class Applicative f <= Selective f where
  select :: forall a b. f (a \/ b) -> f (a -> b) -> f b

infixl 4 select as <*?

selectFlipped :: forall f a b. Selective f => f (a -> b) -> f (a \/ b) -> f b
selectFlipped x y = select y x
infixl 4 selectFlipped as ?*>

selectA :: forall f a b. Applicative f => f (Either a b) -> f (a -> b) -> f b
selectA x y = (\e f -> either f identity e) <$> x <*> y

selectM :: forall f a b. Monad f => f (a \/ b) -> f (a -> b) -> f b
selectM x y = x >>= case _ of
  Left a -> (_ $ a) <$> y
  Right b -> pure b

applyS :: forall f a b. Selective f => f (a -> b) -> f a -> f b
applyS f x = select (Left <$> f) ((#) <$> x)

whenS :: forall f. Selective f => f Boolean -> f Unit -> f Unit
whenS x y = selector <*? effect
  where
  selector = bool (Right unit) (Left unit) <$> x
  effect   = const                         <$> y

branch :: forall f a b c. Selective f => f (a \/ b) -> f (a -> c) -> f (b -> c) -> f c
branch x l r = map (map Left) x <*? map (map Right) l <*? r

ifS :: forall f a. Selective f => f Boolean -> f a -> f a -> f a
ifS x t e = branch selector (const <$> t) (const <$> e)
  where
  selector = bool (Right unit) (Left unit) <$> x

pAnd :: forall f. Selective f => f Boolean -> f Boolean -> f Boolean
pAnd x y = ifS x y (pure false)
infixr 3 pAnd as <&&>

pOr :: forall f. Selective f => f Boolean -> f Boolean -> f Boolean
pOr x y = ifS x (pure true) y
infixr 2 pOr as <||>

fromMaybeS :: forall f a. Selective f => f a -> f (Maybe a) -> f a
fromMaybeS x mx = select (maybe (Left unit) Right <$> mx) (const <$> x)

allS :: forall f t a. Selective f => Foldable t => (a -> f Boolean) -> t a -> f Boolean
allS p = foldr ((<&&>) <<< p) (pure true)

anyS :: forall f t a. Selective f => Foldable t => (a -> f Boolean) -> t a -> f Boolean
anyS p = foldr ((<||>) <<< p) (pure false)

whileS :: forall f. Selective f => f Boolean -> f Unit
whileS x = whenS x (whileS x)

-- | Selective instances for `Const`
newtype Over m a = Over (Const m a)
derive instance functorOver :: Functor (Over m)
derive newtype instance applyOver :: Semigroup m => Apply (Over m)
derive newtype instance applicativeOver :: Monoid m => Applicative (Over m)
instance selectiveOver :: Monoid m => Selective (Over m) where
  select (Over (Const x)) (Over (Const y)) = Over (Const (x <> y))

newtype Under m a = Under (Const m a)
derive instance functorUnder :: Functor (Under m)
derive newtype instance applyUnder :: Semigroup m => Apply (Under m)
derive newtype instance applicativeUnder :: Monoid m => Applicative (Under m)
instance selectUnder :: Monoid m => Selective (Under m) where
  select (Under (Const x)) _ = Under (Const x)

-- | Validation
data Validation e a = Failure e | Success a
derive instance functorValidation :: Functor (Validation e)
instance applyValidation :: Semigroup e => Apply (Validation e) where
  apply (Failure e1) (Failure e2) = Failure (e1 <> e2)
  apply (Failure e1) (Success _)  = Failure e1
  apply (Success _) (Failure e2)  = Failure e2
  apply (Success f) (Success x)   = Success (f x)
instance applictiveValidation :: Semigroup e => Applicative (Validation e) where
  pure = Success
instance selectiveValidation :: Semigroup e => Selective (Validation e) where
  select (Success (Left a)) f  = (_ $ a) <$> f
  select (Success (Right b)) f = Success b
  select (Failure e) _         = Failure e

-- | standard operators used throughout paper
bool :: forall a. a -> a -> Boolean -> a
bool bt bf b = if b then bt else bf
