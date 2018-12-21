module Task where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Profunctor.Choice ((|||))
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, throwError, attempt)

-- | purescript-aff, 3.1.0
-- | `data Aff :: # Effect -> Type -> Type`
-- | An asynchronous computation with effects e. The computation either errors or produces a value of type a.
-- |  is moral equivalent of `ErrorT (ContT Unit (Eff e)) a`.

type Task a = ExceptT Error Aff a

resolve :: forall a. a -> Task a
resolve = pure

reject :: forall a. Error -> Task a
reject = throwError

newTask
  :: forall a
   . ((Either Error a -> Effect Unit) -> Effect Canceler) -> Task a
newTask =
  ExceptT <<< attempt <<< makeAff

toAff :: forall a. Task a -> Aff (Either Error a)
toAff = runExceptT

fork :: forall a r. (Error -> Aff r) -> (a -> Aff r) -> Task a -> Aff r
fork f g t = (f ||| g) =<< toAff t

chain :: forall a b. (a -> Task b) -> Task a -> Task b
chain = (=<<)

res :: forall a. a -> Either Error a
res = Right

rej :: forall a. Error -> Either Error a
rej = Left
