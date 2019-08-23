module Data.Profunctor.Free where

import Prelude

import Data.Exists2 (Exists2, flippedRunExists2, mkExists2)
import Data.Profunctor (class Profunctor)

data FreeP p a b
  = Dimap (Exists2 (DimapF2 p a b))
data DimapF2 p a b x y = DimapF2 (a -> x) (y -> b) (p x y)
mkDimap :: forall p a b x y. (a -> x) -> (y -> b) -> (p x y) -> FreeP p a b
mkDimap l r p = Dimap $ mkExists2 $ DimapF2 l r p

instance profunctorFreePProfucntor :: Profunctor p => Profunctor (FreeP p) where
  dimap l r (Dimap e) = flippedRunExists2 e \(DimapF2 l' r' p') ->
    mkDimap (l' <<< l) (r <<< r') p'
