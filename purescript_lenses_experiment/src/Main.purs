module Main where

import Prelude

import Color (Color)
import Color as Color
import Critter4Us.Model as C4
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens, _Left, _Right, lens, over, set, view)
import Data.Lens.At (at)
import Data.Lens.Common (_1, _2, _Just, _Left)
import Data.Lens.Fold (preview)
import Data.Lens.Index (ix)
import Data.Lens.Prism (prism', review, only, nearly, is)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (traversed, element)
import Data.Lens.Types (Prism')
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Endo (Endo(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (ala, alaF)
import Data.Set as Set
import Data.String (length) as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple4, tuple4)
import Effect (Effect)
import Effect.Console (log, logShow)
import Math (abs) as Math
import Type.Data.Symbol (SProxy(..))

_action :: forall a b r. Lens { action :: a | r } { action :: b | r } a b
_action = lens _.action (_ { action = _ })

_tuple4
  :: forall a1 a2 a3 a4 b4
   . Lens (Tuple4 a1 a2 a3 a4) (Tuple4 a1 a2 a3 b4) a4 b4
_tuple4 = _2 <<< _2 <<< _2 <<< _1

type Percent = Number
data Point = Point Number Number
derive instance genericPoint :: Generic Point _
instance eqPoint :: Eq Point where
  eq x = genericEq x
instance showPoint :: Show Point where
  show (Point x y) = "(" <> show x <> ", "<> show y <> ")"

data Fill
  = Solid Color
  | LinearGradient Color Color Percent
  | RadialGradient Color Color Point
  | NoFill

derive instance genericFill :: Generic Fill _
instance eqFill :: Eq Fill where
  eq x = genericEq x
instance showFill :: Show Fill where
  show x = genericShow x

_solidFill :: Prism' Fill Color
_solidFill =
  prism'
    Solid
    case _ of
      Solid color -> Just color
      _ -> Nothing

_solidFillWhite :: Prism' Fill Unit
_solidFillWhite = only (Solid Color.white)

_solidFillWhite' :: Prism' Fill Unit
_solidFillWhite' =
  nearly -- only needs partial description of an Eq instance
    (Solid Color.white) -- value here (Solid Color.white) doesn't matter, NoFill works as well, sorely injecting the type Solid, can be replaced by Proxy
    case _ of
      Solid color -> color == Color.white
      _ -> false

_first = prop (SProxy :: SProxy "first")
_second = prop (SProxy :: SProxy "second")

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
  logShow $ preview traversed [0, 1, 2]
  logShow $ preview (element 0 traversed) [0, 1, 2]
  logShow $ preview (ix 1) [0, 1, 2]
  logShow $ over (ix 1) negate [0, 1, 2]

  logShow $ set (at 1 <<< _Just) "new value" $ Map.singleton 1 "a"
  logShow $ set (ix 1) "new value" $ Map.singleton 1 "a"

  logShow $ preview _solidFill (Solid Color.white)
  logShow $ set _solidFill (Color.black) (Solid Color.white)
  logShow $ over _solidFill (const Color.black) NoFill
  logShow $ is _solidFill (Solid Color.white) :: Boolean
  logShow $ is _solidFill NoFill :: Boolean
  logShow $ review _solidFillWhite unit
  logShow $ is _solidFillWhite (Solid Color.white) :: Boolean
  logShow $ is _solidFillWhite (Solid Color.black) :: Boolean
  logShow $ is _solidFillWhite NoFill :: Boolean
  logShow $ is _solidFillWhite' (Solid Color.white) :: Boolean

  logShow $ C4.initialModel
  logShow $ C4.addAnimal 0 "wenbo" $ C4.initialModel
  logShow $ C4.addAnimalTag 0 "robot" $ C4.addAnimal 0 "wenbo" $ C4.initialModel

  log $ show $ view _Left (Right 1 :: Either String Int)
  logShow $ is (_Left <<< _Left) (Left $ Left 1) :: Boolean
  logShow $ preview (_first <<< _Right <<< _second <<< _Just ) { first : Right { second : Just 2 }}
