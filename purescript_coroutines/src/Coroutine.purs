module Coroutine where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Identity (Identity)
import Data.Profunctor (class Profunctor)
import Data.Tuple (Tuple)
import Free.Trans (FreeT)

{- | Types -}

type Co = FreeT

type Process = Co Identity

data Emit output next = Emit output next

newtype Await input next = Await (input -> next)

newtype Transform input output next = Transform (input -> Tuple output next)

data CoTransform input output next = CoTransform output (input -> next)

type Producer output = Co (Emit output)

type Comsumer input = Co (Await input)

type Transformer input output = Co (Transform input output)

type CoTransformer input output = Co (CoTransform input output)

{- | Type class instances -}

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
