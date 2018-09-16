module Main where

import Prelude

import Control.Coroutine (($$), ($~), (~$), (~~), (/\), (\/))
import Control.Coroutine as CR
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
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

printer :: CR.Consumer String Aff Unit
printer = forever do
  s <- CR.await
  lift $ liftEffect $ log s
  pure Nothing

showing :: forall a m. Show a => Monad m => CR.Transformer a String m Unit
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
main = Aff.launchAff_ do
  -- CR.runProcess ((nats 0 $~ showing) $$ printer)
  -- CR.runProcess (nats 1 /\ nats 10 $$ showing ~$ printer)
  CR.runProcess (showing `CR.fuseCoTransform` coshowing)

