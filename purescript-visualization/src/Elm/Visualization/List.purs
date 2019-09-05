module Elm.Visualization.List where

import Prelude

import Data.List (List, (:))
import Data.List as List

range :: Number -> Number -> Number -> List Number
range begin end step
  | (end - begin) * step <= 0.0 = List.Nil
  | otherwise = go end begin (- step) List.Nil
  where
    go :: Number -> Number -> Number -> List Number -> List Number
    go s e st rest
      | (s - e) * st >= 0.0 = s : rest
      | otherwise = go (s + st) e st (s : rest)

