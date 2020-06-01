module Main where

import Prelude

import Data.JSDate as Data.JSDate
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console as Effect.Console
import Effect.Ref as Effect.Ref
import Effect.Timer as Effect.Timer
import Hareactive.Interop as Hareactive.Interop
import Hareactive.Types (Stream)

fromArray :: forall a. Array a -> Stream a
fromArray xs =
  let
    producer :: Hareactive.Interop.ProducerFunction a
    producer callback = do
      for_ xs callback
      pure (pure unit)
  in
    Hareactive.Interop.producerStream producer

fromInterval :: Int -> Stream Int
fromInterval interval =
  let
    producer :: Hareactive.Interop.ProducerFunction Int
    producer callback = do
      ref <- Effect.Ref.new 0
      id <- Effect.Timer.setInterval interval do
        count <- Effect.Ref.read ref
        callback count
        Effect.Ref.modify_ (_ + 1) ref
      pure (Effect.Timer.clearInterval id)
  in
   Hareactive.Interop.producerStream producer

main :: Effect Unit
main = do
  let
    ints :: Stream Int
    ints = fromInterval 100
  (Hareactive.Interop.subscribe <@> ints) \x -> do
    jsDate <- Data.JSDate.now
    Effect.Console.log (Data.JSDate.toString jsDate <> ": " <> show x)
  pure unit
