module CofreeVsNu where

import Prelude

-- Nu: 2-rank Type
-- newtype Nu f = Nu (Exists (NuF f)) -- `a` is not given at the top-level scope
-- newtype NuF f a = NuF (Store a (f a)) -- is given at the second-level scope

type Coalgebra f a = a -> f a

--  Tuple :: forall f a. a -> f (Cofree f a) -> Cofree f a
data Cofree f a = Tuple a (f (Cofree f a)) -- Functor f is the Coalgebra

class Functor f <= Corecursive t f | t -> f where
  embed :: f t → t

-- unfold :: forall f s.                          Coalgebra f s   -> s -> Nu f
-- buildCofree :: forall f s a. Functor f => (s -> Tuple a (f s)) -> s -> Cofree f a
unfoldCofree
  :: forall s f a          -- Coalgebra ?
   . Functor f => (s -> a) -> (s -> f s) -> s -> Cofree f a
unfoldCofree e n s =
--Tuple :: forall f a. a -> f (Cofree f a) -> Cofree f a
  Tuple (e s) (unfoldCofree e n <$> n s)
anamorphism :: forall t f a. Corecursive t f => Coalgebra f a -> a -> t
anamorphism n s =
--embed :: forall f t. Corecursive t f => f t → t
                                         -- t = Cofree f a
                                         -- | where is a ? in second-level scope NuF
  embed (anamorphism n <$> n s)
