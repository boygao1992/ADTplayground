module Main where

import Prelude
import Control.Coroutine (Producer, Consumer)
import Control.Coroutine as Control.Coroutine
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Effect.Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Effect.Class.Console
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Immutable as Node.Buffer.Immutable
import Node.Encoding (Encoding(..))
import ZeroMQ.Internal (Address(..))
import ZeroMQ.Internal as ZeroMQ.Internal
import ZeroMQ.Types (Socket)

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
    Effect.Class.Console.log "Publisher bound to port 3000"
    forever do
      Effect.Class.Console.log "sending a multipart message envelope"
      ZeroMQ.Internal.sendMany socket
        [ Node.Buffer.Immutable.fromString "kitty cats" UTF8
        , Node.Buffer.Immutable.fromString "meow!" UTF8
        ]
      Effect.Aff.delay (Effect.Aff.Milliseconds 500.0)

runSubscriber :: Effect Unit
runSubscriber = do
  socket <- ZeroMQ.Internal.newSubscriber
  ZeroMQ.Internal.connect socket address
  Effect.Class.Console.log "Subscriber connected to port 3000"
  ZeroMQ.Internal.subscribeAll socket
  Effect.Aff.launchAff_
    $ Control.Coroutine.runProcess
    $ Control.Coroutine.connect (producer socket) consumer

pushPullExample :: Effect Unit
pushPullExample = do
  runProducer
  runWorker

runProducer :: Effect Unit
runProducer = do
  socket <- ZeroMQ.Internal.newPush
  Effect.Aff.launchAff_ do
    ZeroMQ.Internal.bind socket address
    Effect.Class.Console.log "Producer bound to port 3000"
    forever do
      ZeroMQ.Internal.sendMany socket
        [ Node.Buffer.Immutable.fromString "some work" UTF8 ]
      Effect.Class.Console.log "work sent"
      Effect.Aff.delay (Effect.Aff.Milliseconds 500.0)

runWorker :: Effect Unit
runWorker = do
  socket <- ZeroMQ.Internal.newPull
  ZeroMQ.Internal.connect socket address
  Effect.Class.Console.log "Worker connected to port 3000"
  Effect.Aff.launchAff_
    $ Control.Coroutine.runProcess
    $ Control.Coroutine.connect (producer socket) consumer

producer :: forall from. Socket from -> Producer (Array ImmutableBuffer) Aff Unit
producer socket = Control.Coroutine.producer receiver
  where
  receiver :: Aff (Either (Array ImmutableBuffer) Unit)
  receiver = do
    closed <- liftEffect $ ZeroMQ.Internal.closed socket
    if closed then
      pure $ Right unit
    else
      Left <$> ZeroMQ.Internal.receive socket

consumer :: Consumer (Array ImmutableBuffer) Aff Unit
consumer = Control.Coroutine.consumer emitter
  where
  emitter :: Array ImmutableBuffer -> Aff (Maybe Unit)
  emitter xs = do
    Effect.Class.Console.logShow <<< map (Node.Buffer.Immutable.toString UTF8) $ xs
    pure Nothing
