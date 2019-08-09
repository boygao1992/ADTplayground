module Data.Exists where

import Unsafe.Coerce

foreign import data Exists :: (Type -> Type) -> Type

mkExists :: forall f a. f a -> Exists f
mkExists = unsafeCoerce

runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r
runExists = unsafeCoerce

flippedRunExists :: forall f r. Exists f -> (forall a. f a -> r) -> r
flippedRunExists e f = unsafeCoerce f e
