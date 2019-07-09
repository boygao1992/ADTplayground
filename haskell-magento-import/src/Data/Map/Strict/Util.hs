module Data.Map.Strict.Util where

import RIO
import RIO.List (headMaybe, groupBy)
import qualified Data.Map.Strict as Map

fromListGroupBy :: Ord k => [(k, v)] -> Map.Map k [v]
fromListGroupBy
  = Map.fromList
  . mapMaybe (\(x,y) -> (,y) <$> x)
  . fmap ( (fmap fst . headMaybe) &&& (fmap snd) )
  . groupBy (\x y -> fst x == fst y)
