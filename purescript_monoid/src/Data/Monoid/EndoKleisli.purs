module Data.Monoid.EndoKleisli where

import Prelude

newtype EndoKleisli m a = EndoKleisli (Monad m => a -> m a)

instance semigroupEndoKleisli :: Monad m => Semigroup (EndoKleisli m a) where
  append (EndoKleisli g) (EndoKleisli f) = EndoKleisli (g <=< f)

instance monoidEndoKleisli :: Monad m => Monoid (EndoKleisli m a) where
  mempty = EndoKleisli pure
