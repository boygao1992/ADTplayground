module FreeAp where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

data FreeAp f a
  = Pure a
  | Lift (f a)
-- Ap ( forall i. (FreeAp f (i -> a)) (FreeAp f i) )
  | Ap (FreeAp f (Val -> a)) (FreeAp f Val)

data Val

liftFreeAp :: forall f a. f a -> FreeAp f a
liftFreeAp = Lift

-- private
mkAp :: forall f i a. FreeAp f (i -> a) -> FreeAp f i -> FreeAp f a
mkAp fia fi = Ap (coerceFunc fia) (coerceValue fi)
  where
    coerceFunc :: FreeAp f (i -> a) -> FreeAp f (Val -> a) -- throw away Type i (of an intermediate result) because at this point we are sure that `fia` can be evaluated with `fi`
    coerceFunc = unsafeCoerce

    coerceValue :: FreeAp f i -> FreeAp f Val -- the same
    coerceValue = unsafeCoerce

instance functorFreeAp :: Functor (FreeAp f) where
  map :: forall a b. (a -> b) -> FreeAp f a -> FreeAp f b
  map f x = mkAp (Pure f) x

instance applyFreeAp :: Apply (FreeAp f) where
  apply :: forall i a. FreeAp f (i -> a) -> FreeAp f i -> FreeAp f a
  apply fia fi = mkAp fia fi

instance applictiveFreeAp :: Applicative (FreeAp f) where
  pure :: forall a. a -> FreeAp f a
  pure = Pure

data ConfigF next
  = ConfigFlag String (Boolean -> next)
  | ConfigPort String (Int -> next)
  | ConfigServer String (String -> next)

type DSL a = FreeAp ConfigF a

flag :: String -> DSL Boolean
flag field = liftFreeAp $ ConfigFlag field identity

port :: String -> DSL Int
port field = liftFreeAp $ ConfigPort field identity

server :: String -> DSL String
server field = liftFreeAp $ ConfigServer field identity

type AuthConfig = { port :: Int, host :: String }

authConfig :: Int -> String -> AuthConfig
authConfig p h = { port : p, host : h }
