module Main where

import Prelude

import Critter4Us.Model as C4
import Data.Foldable (class Foldable)
import Data.Foldable (foldMap)
import Data.Lens (Lens, firstOf, lens, over, set, view)
import Data.Lens.At (at)
import Data.Lens.Common (_1, _2)
import Data.Lens.Fold (firstOf)
import Data.Lens.Traversal (traversed, element)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive (..))
import Data.Monoid.Endo (Endo (..))
import Data.Monoid.Multiplicative (Multiplicative (..))
import Data.Newtype (class Newtype, ala, alaF)
import Data.Set as Set
import Data.String (length) as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple4, tuple4)
import Effect (Effect)
import Effect.Console (log, logShow)
import Math (abs) as Math

_action :: forall a b r. Lens { action :: a | r } { action :: b | r } a b
_action = lens _.action (_ { action = _ })

_tuple4
  :: forall a1 a2 a3 a4 b4
   . Lens (Tuple4 a1 a2 a3 a4) (Tuple4 a1 a2 a3 b4) a4 b4
_tuple4 = _2 <<< _2 <<< _2 <<< _1

main :: Effect Unit
main = do

  logShow $ ala Additive foldMap [0, 1, 2]
  logShow $ ala Endo foldMap [ (_ + 1), (_ + 2), (_ + 3)] $ 0
  logShow $ alaF Additive foldMap String.length ["hello", "world"]
  logShow $ alaF Multiplicative foldMap Math.abs [-1.0, -2.0, -3.0]

  logShow $ view _action { action: "view" }
  logShow $ over _action (const "over") { action: "no action" }
  logShow $ set _action "set" { action: "no action"}

  logShow $ set _tuple4 0 (tuple4 1 2 3 4)

  logShow $ set (at "hello") (Just "world") Map.empty
  logShow $ view (at "hello") $ Map.fromFoldable [Tuple "hello" "world"]

  logShow $ view (at 1) $ Set.fromFoldable [1, 2, 3]
  logShow $ view (at 0) $ Set.fromFoldable [1, 2, 3]

  logShow $ over traversed (_ + 1) [0, 1, 2]
  logShow $ over (element 0 traversed) (_ + 1) [0, 2, 3]
  logShow $ view traversed ["M", "o", "noid"]
  logShow $ view traversed [[0], [1], [2]]
  logShow $ firstOf (element 0 traversed) [0, 1, 2]

  logShow $ C4.initialModel
  logShow $ C4.addAnimal 0 "wenbo" $ C4.initialModel
  logShow $ C4.addAnimalTag 0 "robot" $ C4.addAnimal 0 "wenbo" $ C4.initialModel

