module Coroutine where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple(..))
import Free.Trans (FreeT, liftFreeT, runFreeT)

{- | Types -}

type Co = FreeT

newtype Identity next = Identity next -- an Algebra Functor doing nothing other than repacking

data Emit output next = Emit output next

newtype Await input next = Await (input -> next)

newtype Transform input output next = Transform (input -> Tuple output next)

data CoTransform input output next = CoTransform output (input -> next)

type Process = Co Identity

type Producer output = Co (Emit output)

type Consumer input = Co (Await input)

type Transformer input output = Co (Transform input output)

type CoTransformer input output = Co (CoTransform input output)

{- | Type class instances -}

derive instance functorIdentity :: Functor Identity

instance functorEmit :: Functor (Emit output) where
  map f (Emit output next) = Emit output (f next)

instance bifunctorEmit :: Bifunctor Emit where
  bimap f g (Emit output next) = Emit (f output) (g next)

instance functorAwait :: Functor (Await input) where
  map f (Await reply) = Await (f <<< reply)

instance profunctorAwait :: Profunctor Await where
  dimap f g (Await reply) = Await (g <<< reply <<< f)

instance functorTransform :: Functor (Transform input output) where
  map f (Transform tf) = Transform (map f <<< tf)

instance bifunctorTransform :: Bifunctor (Transform input) where
  bimap f g (Transform tf) = Transform (bimap f g <<< tf)

instance functorCoTransform :: Functor (CoTransform input output) where
  map f (CoTransform output reply) = CoTransform output (f <<< reply)

instance bifunctorCoTransform :: Bifunctor (CoTransform input) where
  bimap f g (CoTransform output reply) = CoTransform (f output) (g <<< reply)

{- | Interpreters -}

interpIdentity :: forall m. Monad m => Identity ~> m -- a trivial interpreter
interpIdentity (Identity next) = pure next

{- | Utils -}
loop
  :: forall f m a
   . Functor f
  => Monad m
  => Co f m (Maybe a)
  -> Co f m a
loop co = tailRecM go unit
  where
    go :: Unit -> Co f m (Step Unit a)
    go = const $ co <#> -- maybe (Loop unit) Done
      case _ of
        Just return -> Done return
        Nothing     -> Loop unit

{- | DSL Constructors -}

emit :: forall m output. Monad m => output -> Producer output m Unit
emit output = liftFreeT $ Emit output unit

await :: forall m input. Monad m => Consumer input m input
await = liftFreeT $ Await identity

transform :: forall m input output. Monad m => (input -> output) -> Transformer input output m Unit
transform f = liftFreeT $ Transform $ \input -> Tuple (f input) unit

cotransform :: forall m input output. Monad m => output -> CoTransformer input output m input
cotransform output = liftFreeT $ CoTransform output identity

{- | Coroutine Constructors -}
producer
  :: forall m output return
   . Monad m
  => m (Either output return)
  -> Producer output m return
producer recv = loop do
  e <- lift recv
  case e of
    Left output -> do
      emit output
      pure Nothing
    Right return ->
      pure $ Just return

consumer
  :: forall m input return
   . Monad m
  => (input -> m (Maybe return))
  -> Consumer input m return
consumer send = loop do
  input <- await
  lift $ send input

{- Executors -}

runProcess :: forall m a. MonadRec m => Process m a -> m a
runProcess = runFreeT interpIdentity
