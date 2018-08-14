module Cofree where

import Prelude

data Cofree w a
  = Tuple a (w (Cofree w a))

instance functorCofree :: Functor w => Functor (Cofree w) where
  -- map :: forall a b. (a -> b) -> w a -> w b
  map g (Tuple a wca) = Tuple (g a) (map (map g) wca)

-- instance extendCofree :: Functor w => Extend (Cofree w) where
  -- extend :: forall b a . (w a -> b) -> w a -> w b
