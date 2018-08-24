{-# LANGUAGE
RankNTypes,
TupleSections
#-}

module ListLens where

import Prelude
import Data.Functor.Compose

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

-- data Storey x f a = Storey x (f a)
-- instance Functor f => Functor (Storey x f) where
--   fmap f (Storey x fa) = Storey x (fmap f fa)

-- newtype Compose f g a = Compose { getCompose :: f (g a) }
-- | Right-to-left composition of functors
-- instance (Functor f, Functor g) => Functor (Compose f g)
-- instance Functor ((,) x)
                   -- f = ((,) x)
                           -- g = f
                                                -- f (g a) = (x, f a)
type Storey x f = Compose ((,) x) f

ix :: Int -> Lens [a] a
ix _ _ [] = error "ix: index too large"
ix index f list
  | index < 0 = error "ix: negative index"
  | old : rest <- list =
      if index == 0
      then (: rest) <$> f old
      else (old :) <$> ix (index-1) f rest

