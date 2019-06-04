module Data.List.Util where

import RIO

import qualified Data.Set as Set

returningOne :: [a] -> Maybe a
returningOne [x] = Just x
returningOne _ = Nothing

nub :: Ord a => [a] -> [a]
nub = Set.toList . Set.fromList
