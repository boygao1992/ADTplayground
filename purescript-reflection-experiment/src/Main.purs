module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)

import Data.Reflection (class Reifies, reflect, reify)
import Data.Newtype (class Newtype, unwrap)
import Type.Proxy (Proxy(..))

arrayEq :: forall a. Eq a => Array a -> Array a -> Boolean
arrayEq = eq

newtype E s a = E a
derive instance newtypeE :: Newtype (E s a) _
newtype Eq_ a = Eq_ (a -> a -> Boolean)
derive instance newtypeEq_ :: Newtype (Eq_ a) _

instance reifiesEqE :: Reifies s (Eq_ a) => Eq (E s a) where
  eq a b = (unwrap (reflect (Proxy :: Proxy s))) (unwrap a) (unwrap b)

withEq :: forall a. (a -> a -> Boolean) -> (forall s. Reifies s (Eq_ a) => E s a) -> a
withEq f v = reify (Eq_ f) (unwrap <<< asProxyOf v)
  where
    asProxyOf :: forall s. E s a -> Proxy s -> E s a
    asProxyOf v' _ = v'

newtype O a s = O a
derive instance newtypeO :: Newtype (O a s) _
newtype Ord_ a = Ord_ (a -> a -> Ordering)
derive instance newtypeOrd_ :: Newtype (Ord_ a) _

isEq :: Ordering -> Boolean
isEq EQ = true
isEq _  = false

instance reifiesEqO :: Reifies s (Ord_ a) => Eq (O a s) where
  eq a b = isEq ( (unwrap (reflect (Proxy :: Proxy s))) (unwrap a) (unwrap b))
instance reifiesOrdO :: (Eq (O a s), Reifies s (Ord_ a)) => Ord (O a s) where
  compare a b = unwrap (reflect (Proxy :: Proxy s)) (unwrap a) (unwrap b)

withOrd :: forall a. (a -> a -> Ordering) -> (forall s. Reifies s (Ord_ a) => O a s) -> a
withOrd f v = reify (Ord_ f) (unwrap <<< asProxyOf v)
  where
    asProxyOf :: forall f s. f s -> Proxy s -> f s
    asProxyOf v' _ = v'

example1 :: Int
example1 = withOrd compare (max (O 1) (O 2))

example2 :: Int
example2 = withOrd (flip compare) (max (O 1) (O 2))

-- example3 :: Boolean
-- example3 = reifyEq (\_ _ -> true) (\_ -> arrayEq [ReflectedEq 1] [ReflectedEq 2])

main :: Effect Unit
main = do
  logShow example

  logShow example1
  logShow example2

  where
    example :: Int
    example = reify 10 \p -> (reflect p) + (reflect p)
