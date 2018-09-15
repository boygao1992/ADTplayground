module Main where

import Prelude

-- import AVar as AVar
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Console (log)

main :: Effect Unit
main = do
  Aff.launchAff_ do
    var <- AVar.empty

    _ <- Aff.forkAff do
      i <- AVar.take var
      liftEffect $ log $ "take " <> show i

    _ <- Aff.forkAff do
      i <- AVar.take var
      liftEffect $ log $ "take " <> show i

    _ <- Aff.forkAff do
      Aff.delay $ Aff.Milliseconds 1000.0
      AVar.put 1 var
      liftEffect $ log $ "put 1"

    _ <- Aff.forkAff do
      Aff.delay $ Aff.Milliseconds 2000.0
      AVar.put 2 var
      liftEffect $ log $ "put 2"

    pure unit
