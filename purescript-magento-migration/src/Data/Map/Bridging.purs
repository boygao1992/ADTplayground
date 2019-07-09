module Data.Map.Bridging
( Bridging(..)
, _bridged
, bridging
) where


import Prelude

import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Lens (Prism', preview)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Profunctor.Strong ((&&&))
import Generic.Optic (_Ctor')
import Type.Data.Symbol (SProxy(..))

data Bridging a b
  = SourceOnly a
  | TargetOnly b
  | Bridged { left :: a, right :: b }
  | InvalidBridging
derive instance genericBridging :: Generic (Bridging a b) _

_bridged :: forall a b. Prism' (Bridging a b) { left :: a, right :: b }
_bridged = _Ctor' (SProxy :: SProxy "Bridged")

bridging
  :: forall f g s t key
  .  Foldable f
  => Functor f
  => Foldable g
  => Functor g
  => Ord key
  => f s
  -> (s -> key)
  -> g t
  -> (t -> key)
  -> List { left :: s, right :: t }
bridging source sourceAccessor target targetAccessor =
  List.mapMaybe (preview _bridged)
  <<< List.fromFoldable
  $ Map.unionWith (<>)
  ( Map.fromFoldable
    <<< map (sourceAccessor &&& SourceOnly)
    $ source
  )
  ( Map.fromFoldable
    <<< map (targetAccessor &&& TargetOnly)
    $ target
  )

instance showBridging :: (Show a, Show b) => Show (Bridging a b) where
  show (SourceOnly x) = show x <> " ~ ?"
  show (TargetOnly y) = "? ~ " <> show y
  show (Bridged { left: x, right: y}) = show x <> " ~ " <> show y
  show InvalidBridging = "? ~ ?"

instance semigroupBridging :: Semigroup (Bridging a b) where
  append (SourceOnly x) (TargetOnly y) = Bridged {left: x, right: y}
  append _ _ = InvalidBridging

instance monoidBridging :: Monoid (Bridging a b) where
  mempty = InvalidBridging
