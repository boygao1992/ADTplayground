module Utils where

import Prelude

import Data.Int (fromNumber)
import Data.Maybe (Maybe)
import Global (readInt)

parseInt :: Int -> String -> Maybe Int
parseInt base = fromNumber <<< readInt base
