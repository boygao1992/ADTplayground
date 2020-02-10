module Main where

import Prelude
import Control.Monad.Rec.Class (Step(..), forever, tailRecM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Effect.Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Effect.Console
import Node.Buffer.Immutable as Node.Buffer.Immutable
import Node.Encoding (Encoding(..))
import ZeroMQ.Internal (Address(..))
import ZeroMQ.Internal as ZeroMQ.Internal

main :: Effect Unit
main = pushPullExample

address = Address "tcp://127.0.0.1:3000" :: Address

pubSubExample :: Effect Unit
pubSubExample = do
  runPublisher
  runSubscriber

runPublisher :: Effect Unit
runPublisher = do
  socket <- ZeroMQ.Internal.newPublisher
  Effect.Aff.launchAff_ do
    ZeroMQ.Internal.bind socket address
    Effect.Console.log "Publisher bound to port 3000"
    forever do
      Effect.Console.log "sending a multipart message envelope"
      ZeroMQ.Internal.sendMany socket
        [ Node.Buffer.Immutable.fromString "kitty cats" UTF8
        , Node.Buffer.Immutable.fromString "meow!" UTF8
        ]
      Effect.Aff.delay (Effect.Aff.Milliseconds 500.0)

runSubscriber :: Effect Unit
runSubscriber = do
  socket <- ZeroMQ.Internal.newSubscriber
  ZeroMQ.Internal.connect socket address
  Effect.Console.log "Subscriber connected to port 3000"
  ZeroMQ.Internal.subscribeAll socket
  Effect.Aff.launchAff_
    $ tailRecM
        ( \mx -> do
            case mx of
              Nothing -> pure unit
              Just xs ->
                Effect.Console.logShow
                  <<< map (Node.Buffer.Immutable.toString UTF8)
                  $ xs
            closed <- liftEffect $ ZeroMQ.Internal.closed socket
            if closed then
              pure $ Done unit
            else
              Loop <<< Just <$> ZeroMQ.Internal.receive socket
        )
        Nothing

pushPullExample :: Effect Unit
pushPullExample = do
  runProducer
  runWorker

runProducer :: Effect Unit
runProducer = do
  socket <- ZeroMQ.Internal.newPush
  Effect.Aff.launchAff_ do
    ZeroMQ.Internal.bind socket address
    Effect.Console.log "Producer bound to port 3000"
    forever do
      ZeroMQ.Internal.sendMany socket
        [ Node.Buffer.Immutable.fromString "some work" UTF8 ]
      Effect.Console.log "work sent"
      Effect.Aff.delay (Effect.Aff.Milliseconds 500.0)

runWorker :: Effect Unit
runWorker = do
  socket <- ZeroMQ.Internal.newPull
  ZeroMQ.Internal.connect socket address
  Effect.Console.log "Worker connected to port 3000"
  Effect.Aff.launchAff_
    $ tailRecM
        ( \mx -> do
            case mx of
              Nothing -> pure unit
              Just xs ->
                Effect.Console.logShow
                  <<< map (Node.Buffer.Immutable.toString UTF8)
                  $ xs
            closed <- liftEffect $ ZeroMQ.Internal.closed socket
            if closed then
              pure $ Done unit
            else
              Loop <<< Just <$> ZeroMQ.Internal.receive socket
        )
        Nothing
