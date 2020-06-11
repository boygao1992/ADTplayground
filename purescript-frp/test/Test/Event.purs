module Test.Event (suite) where

import Prelude
import Data.Array as Data.Array
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Effect.Ref
import FRP as FRP
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Event" do
    Test.Unit.suite "subscribe" do
      Test.Unit.test "supports multiple listeners" do
        actual <-
          liftEffect do
            s :: FRP.SinkEvent Int <- FRP.sinkEvent
            spy1 <- subscribeSpy s.event
            spy2 <- subscribeSpy s.event
            s.push 2
            s.push 3
            { spy1: _
            , spy2: _
            }
              <$> spy1.received
              <*> spy2.received
        let
          expected =
            { spy1: [ 2, 3 ]
            , spy2: [ 2, 3 ]
            }
        Test.Unit.Assert.equal expected actual
      Test.Unit.test "single listeners can be removed" do
        actual <-
          liftEffect do
            s :: FRP.SinkEvent Int <- FRP.sinkEvent
            spy1 <- subscribeSpy s.event
            spy2 <- subscribeSpy s.event
            spy2.unsubscribe
            s.push 2
            s.push 3
            { spy1: _
            , spy2: _
            }
              <$> spy1.received
              <*> spy2.received
        let
          expected =
            { spy1: [ 2, 3 ]
            , spy2: []
            }
        Test.Unit.Assert.equal expected actual
      Test.Unit.test "supports removing listener when more than two" do
        actual <-
          liftEffect do
            s :: FRP.SinkEvent Int <- FRP.sinkEvent
            spy1 <- subscribeSpy s.event
            spy2 <- subscribeSpy s.event
            spy3 <- subscribeSpy s.event
            spy2.unsubscribe
            s.push 2
            s.push 3
            { spy1: _
            , spy2: _
            , spy3: _
            }
              <$> spy1.received
              <*> spy2.received
              <*> spy3.received
        let
          expected =
            { spy1: [ 2, 3 ]
            , spy2: []
            , spy3: [ 2, 3 ]
            }
        Test.Unit.Assert.equal expected actual

subscribeSpy ::
  forall a.
  FRP.Event a ->
  Effect
    { received :: Effect (Array a)
    , unsubscribe :: Effect Unit
    }
subscribeSpy event = do
  { callback, received } <- spy
  { unsubscribe } <- FRP.subscribe event callback
  pure
    { received
    , unsubscribe
    }

spy :: forall a. Effect { callback :: a -> Effect Unit, received :: Effect (Array a) }
spy = do
  receivedRef <- Effect.Ref.new []
  let
    callback :: a -> Effect Unit
    callback a = Effect.Ref.modify_ (_ `Data.Array.snoc` a) receivedRef

    received :: Effect (Array a)
    received = Effect.Ref.read receivedRef
  pure
    { callback
    , received
    }
