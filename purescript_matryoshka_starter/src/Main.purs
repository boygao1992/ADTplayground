module Main where

import Prelude

import Control.Comonad.Store (Store, runStore, store)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- | unsafe coerce
foreign import unsafeCoerce :: forall a b. a -> b

-- | existential extension
foreign import data Exists :: (Type -> Type) -> Type

mkExists :: forall f a. f a -> Exists f -- forget `a` during data construction / delay asking for `a`
mkExists = unsafeCoerce

runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r -- ask for `a` when evaluating `f a` (Type of `a` carried by function `f a -> r`)
runExists = unsafeCoerce

-- | Mu
newtype Mu f = In (f (Mu f))

instance showMu :: (Show (f TacitString), Functor f) => Show (Mu f) where
  show (In x) = show <<< map (hush <<< show) $ x

roll :: forall f. f (Mu f) -> Mu f -- Fix :: f (Fix f) -> Fix f
roll = In

unroll :: forall f. Mu f -> f (Mu f) -- unFix :: Fix f -> f (Fix f)
unroll (In x) = x

-- | Nu
newtype NuF f a = NuF (Store a (f a))
newtype Nu f = Nu (Exists (NuF f)) -- forall a. Store a (f a)

unfold :: forall f a. a -> (a -> f a) -> Nu f
unfold pos peek = Nu $ mkExists $ NuF $ store peek pos

observe :: forall f. Functor f => Nu f -> f (Nu f)
observe (Nu ex) = runExists observeF ex
  where
    observeF :: forall a. Functor f => NuF f a -> f (Nu f)
    observeF (NuF x) =
      flip unfold peek <$> peek pos
        where
          Tuple peek pos = runStore x

-- | TacitString
newtype TacitString = TacitString String
instance showTacitString :: Show TacitString where
  show (TacitString str) = str
hush :: String -> TacitString
hush = TacitString

-- | Recursive
class Functor f <= Recursive t f | t -> f where
  project :: t -> f t -- unFix

instance recursiveMu :: Functor f => Recursive (Mu f) f where
  project = unroll

-- | Corecursive
class Functor f <= Corecursive t f | t -> f where
  embed :: f t -> t -- Fix

instance corecursiveMu :: Functor f => Corecursive (Mu f) f where
  embed = roll

-- | Algebra
type Algebra f a = f a -> a

-- | Coalgebra
type Coalgebra f a = a -> f a

-- | Catamorphism
cata :: forall t f a. Recursive t f => Algebra f a -> t -> a
 -- Algebra f a . fmap (cata (Algebra f a)) . unFix
cata alg = alg <<< map (cata alg) <<< project

-- | Anamorphism
ana :: forall t f a. Corecursive t f => Coalgebra f a -> a -> t
ana coalg = embed <<< map (ana coalg) <<< coalg

-- | Hylomorphism
hylo :: forall f a b. Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = alg <<< map (hylo alg coalg) <<< coalg

-- | List

data ListF a t = Nil | Cons a t
derive instance functorListF :: Functor (ListF a)

instance showListF :: Show a => Show (ListF a TacitString) where
  show Nil = "Nil"
  show (Cons h t) = show h <> " : " <> show t

nil :: forall a t. Corecursive t (ListF a) => t
nil = embed Nil

cons :: forall a t. Corecursive t (ListF a) => a -> t -> t
cons h t = embed (Cons h t)

prod :: Algebra (ListF Int) Int
prod Nil = 1
prod (Cons h t) = h * t

countDown :: Coalgebra (ListF Int) Int
countDown n
  | n <= 0 = Nil
  | otherwise = Cons n (n - 1)

factorial :: Int -> Int
factorial = hylo prod countDown

type List a = Mu (ListF a)

main :: Effect Unit
main = do
  let someList = cons 1 $ cons 2 $ cons 3 $ cons 4 $ (nil :: List Int)
  log <<< show $ someList
  log <<< show <<< cata prod $ someList
  log <<< show $ (ana countDown 4 :: List Int)
  log <<< show $ factorial 6
