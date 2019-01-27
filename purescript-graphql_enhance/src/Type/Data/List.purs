module Type.Data.List where

foreign import kind List
foreign import data Nil :: List
foreign import data Cons :: Type -> List -> List

