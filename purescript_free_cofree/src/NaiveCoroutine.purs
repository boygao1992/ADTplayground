module NaiveCoroutine where

import Prelude

import Control.Monad.Free
 -- implementation in lazy language
 -- data Free f a
 --   = Pure a
 --   | Roll f (Free f a)

 -- data Val
 -- data FreeView f a b
 --   = Return a
 --   | Bind (f b) (b -> Free f a)
 -- newtype ExpF f = ExpF (Val -> Free f Val)
 -- data Free f a = Free (FreeView f Val Val) (CatList (ExpF f))
  ( Free
 -- liftF :: forall f. f ~> (Free f)
  , liftF) as F

import Data.Exists (Exists(..))

data Emit o a = Emit o a

type Producer o = F.Free (Emit o)

emit :: forall o. o -> Producer o Unit
emit o = F.liftF $ Emit o unit

emit3 :: Producer Int Unit
emit3 = do
  emit 1
  emit 2
  emit 3

-- data Bound f m b a = Bound (Unit -> FreeT f m a) (a -> FreeT f m b)
-- data FreeT f m a
--   = FreeT (Unit -> m (Either a (f (FreeT f m a))))
--   | Bind (Exists (Bound f m a))

data GosubF f a b = GosubF (Unit -> Free f b) (b -> Free f a)
data Free f a
  = Pure a
  | Free (f (Free f a))
  | Gosub (Exists (GosubF f a)) -- Monadic bind encoded in sequential data structure, equivalent to the following:
-- Gosub (forall b. GosubF f a b)
