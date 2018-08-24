{-# LANGUAGE
RankNTypes,
TupleSections
#-}

module ListLens where

import Prelude
import Data.Bifunctor

type Lens s a = forall f. Functor f => (a -> f a) -> s -> (a, f s)

ix :: Int -> Lens [a] a
ix _ _ [] = error "ix: index too large"
ix index f list
  | index < 0 = error "ix: negative index"
  | old : rest <- list =
      if index == 0
      then (old, (: rest) <$> f old)
      else second ((old :) <$>) $ ix (index-1) f rest

