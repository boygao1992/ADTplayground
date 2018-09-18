module Main where

import Prelude

import Control.Coroutine (($$), ($~), (~$), (~~), (/\), (\/))
import Control.Coroutine as CR
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Class as State
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)


nats :: Int -> CR.Producer Int Aff Unit
nats = go
  where
    go :: Int -> CR.Producer Int Aff Unit
    go i = do
      CR.emit i
      lift $ Aff.delay $ Aff.Milliseconds 500.0
      -- go (i + 1)

zeros :: CR.Producer Int Aff Unit
zeros = CR.producer recv
  where
    recv :: Aff (Either Int Unit)
    recv = do
      Aff.delay $ Aff.Milliseconds 500.0
      pure $ Left 0

printer :: forall r. CR.Consumer String Aff r
printer = forever do
  s <- CR.await
  lift $ liftEffect $ log s
  pure Nothing

showing :: forall a m r. Show a => Monad m => CR.Transformer a String m r
showing = forever $ CR.transform show

coshowing :: CR.CoTransformer String Int Aff Unit
coshowing = go 0
  where
    go :: Int -> CR.CoTransformer String Int Aff Unit
    go i = do
      o <- CR.cotransform i
      lift $ liftEffect $ log o
      lift $ Aff.delay $ Aff.Milliseconds 500.0
      go (i + 1)

main :: Effect Unit
main = do
  Aff.launchAff_ do
    -- CR.runProcess ((nats 0 $~ showing) $$ printer)
    -- CR.runProcess (nats 1 /\ nats 10 $$ showing ~$ printer)
    -- CR.runProcess (showing `CR.fuseCoTransform` coshowing)
    CR.runProcess (zeros $$ showing ~$ printer)

