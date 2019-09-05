module Elm.Visualization.Scale.Sequantial where

import Prelude

convert :: forall a. Number -> Number -> (Number -> a) -> Number -> a
convert x0 x1 interpolator x =
  interpolator ((x - x0) / (x1 - x0))
