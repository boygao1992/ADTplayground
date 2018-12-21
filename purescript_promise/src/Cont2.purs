module Cont2 where

import Prelude
import Data.Newtype (class Newtype, unwrap)

newtype Cont r a = Cont ((a -> r) -> r)
derive instance newtypeCont :: Newtype (Cont r a) _

runCont :: forall r a. Cont r a -> (a -> r) -> r
runCont = unwrap

instance functorCont :: Functor (Cont r) where
  map f (Cont c) = Cont (\l -> c (l <<< f))

instance applyCont :: Apply (Cont r) where
  apply (Cont f) (Cont c) = Cont \l -> f(\k -> c ( l <<< k ))

instance applicativeCont :: Applicative (Cont r) where
  pure x = Cont \f -> f x

instance bindCont :: Bind (Cont r) where
  bind (Cont c) k = Cont \l -> c ( \a -> unwrap(k a) l)

instance monadCont :: Monad (Cont r)
