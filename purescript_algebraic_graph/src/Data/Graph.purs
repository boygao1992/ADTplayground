module Data.Graph where

-- [An algebra of graphs](https://blogs.ncl.ac.uk/andreymokhov/an-algebra-of-graphs/)

import Prelude

import Data.Tuple (Tuple (..))
import Data.Foldable (foldr)

-- import Foldable

class Graph g v | g -> v where
  empty :: g
  vertex :: v -> g
  overlay :: g -> g -> g -- (+)
  connect :: g -> g -> g -- (->)

{-
laws
- Decomposition axiom (which differentiates Algebraic Graph from Semiring)
  - x -> y -> z = (x -> y) + (x -> z) + (y -> z)
- `empty` is the identity element of both overlay and connect
  - Prop: oe = ce (overlayMempty = oe, connectMempty = ce)
    - Known
      1. oe + a = a = a + oe
      2. ce -> a = a = a -> ce
    - Proof
      - oe
      - = oe -> ce
      - = oe -> ce -> ce
      - = (oe -> ce) + (oe -> ce) + (ce -> ce) (decomposition axiom)
      - = oe + oe + ce
      - = oe + ce
      - = ce
- Idempotent commutative monoid under overlay (G, +, empty)
  - Idempotence: a + a = a
  - Associativity: (a + b) + c = a + (b + c)
  - Identity: empty + a = a = a + empty
  - Commutativity: a + b = b + a
- Monoid under connect (G, ->, empty)
  - Associativity: (a -> b) -> c = a -> (b -> c)
  - Identity: empty -> a = a -> empty
- Connect distributes over overlay
  - Left distributivity: a -> (b + c) = (a -> b) + (a -> c)
  - Right distributivity: (a + b) -> c = (a -> c) + (b -> c)
-}

{-
class Semiring a where
  zero :: a
  add :: a -> a -> a
  one :: a
  mul :: a -> a -> a

infixl add 6 as +
infixl mul 7 as *

laws
- Commutative monoid under addition (S, +, 0)
  - Associativity: (a + b) + c = a + (b + c)
  - Identity: 0 + a = a = a + 0
  - Commutativity: a + b = b + a
- Monoid under multiplication (S, *, 1)
  - Associativity: (a * b) * c = a * (b * c)
  - Identity: 1 * a = a * 1
  - (not necessary Commutative, e.g. Vector Space)
- Multiplication distributes over addition
  - Left distributivity: a * (b + c) = (a * b) + (a * c)
  - Right distributivity: (a + b) * c = (a * c) + (b * c)
-}

vertices :: forall g v. Graph g v => Array v -> g
vertices = foldr overlay empty <<< map vertex

clique :: forall g v. Graph g v => Array v -> g
clique = foldr connect empty <<< map vertex

fromEdgeList :: forall g v. Graph g v => Array (Tuple v v) -> g
fromEdgeList = foldr overlay empty <<< map edge
  where
    edge (Tuple x y) = vertex x `connect` vertex y
