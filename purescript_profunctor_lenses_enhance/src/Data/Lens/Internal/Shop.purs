module Data.Lens.Internal.Shop where

import Prelude

import Data.Profunctor (class Profunctor)

data Shop a b s t = Shop (s -> a) (s -> b -> t)

instance profunctorShop :: Profunctor (Shop a b) where
  dimap :: forall v w s t. (v -> s) -> (t -> w) -> Shop a b s t -> Shop a b v w
  dimap pre post (Shop get set)
    = Shop
        (get <<< pre) -- :: v -> a
        (\v -> post <<< set (pre v)) -- :: v -> b -> w

-- TODO instance Strong (Shop a b)
