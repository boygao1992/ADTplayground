module Data.Lens.Effect.Ref where

import Prelude

import Control.Monad.Free (Free, liftF)
import Control.Monad.Reader.Trans (class MonadAsk, class MonadReader, ReaderT(..))
import Data.Lens (Lens, lens)
import Data.Lens.Internal.Shop (Shop)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

refGets :: forall s a. (s -> a) -> Ref s -> Effect a
refGets getter ref = getter <$> Ref.read ref

refSets :: forall s a. (s -> a -> s) -> Ref s -> a -> Effect s
refSets setter ref a = Ref.modify (setter <@> a) ref


{- NOTE lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
  s = Ref s
  t = Effect s
  a = Effect a
  b = a
-}
{- FIXME not horizontally composable anymore
why? not (weakly) idempotent: (Effect a) /~ (Ref s)
Strong p => p (Effect a) a -> p (Ref s) (Effect s)
-}
refLens' :: forall s a. (s -> a) -> (s -> a -> s) -> Lens (Ref s) (Effect s) (Effect a) a
refLens' getter setter = lens (refGets getter) (refSets setter)

{- NOTE solution: capture actions by an eDSL which is pure
-}

data ExpF s a next
  = Gets (a -> next)
  | Sets a (s -> next)

type Exp s a = Free (ExpF s a)

newtype RefProgram s a next = RefProgram (ReaderT (Shop a a s s) (Exp s a) next)

derive instance functorRefProgram :: Functor (RefProgram s a)
derive newtype instance applyRefProgram :: Apply (RefProgram s a)
derive newtype instance applicativeRefProgram :: Applicative (RefProgram s a)
derive newtype instance bindRefProgram :: Bind (RefProgram s a)
derive newtype instance monadRefProgram :: Monad (RefProgram s a)
derive newtype instance monadAskRefProgram ::
  MonadAsk (Shop a a s s) (RefProgram s a)
derive newtype instance monadReaderRefProgram ::
  MonadReader (Shop a a s s) (RefProgram s a)

gets :: forall s a. RefProgram s a a
gets = RefProgram $ ReaderT $ \_ -> liftF $ Gets identity

sets :: forall s a. a -> RefProgram s a s
sets a = RefProgram $ ReaderT $ \_ -> liftF $ Sets a identity

interpret :: forall s a. (s -> a) -> (s -> a -> s) -> Ref s -> ExpF s a ~> Effect
interpret getter setter ref = case _ of
  Gets reply -> reply <$> refGets getter ref
  Sets a reply -> reply <$> refSets setter ref a

program :: forall s a. RefProgram s a Unit
program = do
  a <- gets
  void $ sets a

-- (s -> a) -> (s -> a -> s) -> RefProgram a b next -> RefProgram s b next
-- LensEnv a b ~ (a -> b, a -> b -> a)
-- LensEnv s b ~ (s -> b, s -> b -> s)
-- s -> b = getter <<< wrapGetter
-- s -> b -> s = \s -> wrapSetter s <<< setter (wrapGetter s)




