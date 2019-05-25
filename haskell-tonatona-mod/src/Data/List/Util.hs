module Data.List.Util where

import RIO

returningOne :: [a] -> Maybe a
returningOne [x] = Just x
returningOne _ = Nothing
