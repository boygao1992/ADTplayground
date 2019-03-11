module Data.Lens.Internal.Market where

import Prelude

import Data.Bifunctor (lmap)
import Data.Profunctor (class Profunctor)
import Data.Either (Either)

data Market a b s t = Market (b -> t) (s -> Either t a)

instance profunctorMarket :: Profunctor (Market a b) where
  dimap :: forall v w s t. (v -> s) -> (t -> w) -> Market a b s t -> Market a b v w
  dimap pre post (Market set get)
    = Market
        (post <<< set) -- :: v -> a
        -- lmap post :: Either t a -> Either w a
        (lmap post <<< get <<< pre) -- :: v -> Either w a
