module Test.Event (suite) where

import Prelude
import Control.Alt ((<|>))
import Control.Plus as Control.Plus
import Data.Array as Data.Array
import Data.Traversable as Data.Traversable
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP as FRP
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert
import Test.Util as Test.Util

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Event" do
    testAlt
    testEmpty
    testMap
    testFilter

testSubscribe :: Test.Unit.TestSuite
testSubscribe =
  Test.Unit.suite "subscribe" do
    Test.Unit.test "supports multiple subscribers" do
      actual <-
        liftEffect do
          s :: FRP.SinkEvent Int <- FRP.sinkEvent
          spy1 <- Test.Util.subscribeSpy s.event
          spy2 <- Test.Util.subscribeSpy s.event
          Data.Traversable.for_ [ 0, 1 ] s.push
          { spy1: _
          , spy2: _
          }
            <$> spy1.received
            <*> spy2.received
      let
        expected =
          { spy1: [ 0, 1 ]
          , spy2: [ 0, 1 ]
          }
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "single subscriber can be removed" do
      actual <-
        liftEffect do
          s :: FRP.SinkEvent Int <- FRP.sinkEvent
          spy1 <- Test.Util.subscribeSpy s.event
          spy2 <- Test.Util.subscribeSpy s.event
          spy2.unsubscribe
          Data.Traversable.for_ [ 0, 1 ] s.push
          { spy1: _
          , spy2: _
          }
            <$> spy1.received
            <*> spy2.received
      let
        expected =
          { spy1: [ 0, 1 ]
          , spy2: []
          }
      Test.Unit.Assert.equal expected actual
    Test.Unit.test "supports removing subscriber when more than two" do
      actual <-
        liftEffect do
          s :: FRP.SinkEvent Int <- FRP.sinkEvent
          spy1 <- Test.Util.subscribeSpy s.event
          spy2 <- Test.Util.subscribeSpy s.event
          spy3 <- Test.Util.subscribeSpy s.event
          spy2.unsubscribe
          Data.Traversable.for_ [ 0, 1 ] s.push
          { spy1: _
          , spy2: _
          , spy3: _
          }
            <$> spy1.received
            <*> spy2.received
            <*> spy3.received
      let
        expected =
          { spy1: [ 0, 1 ]
          , spy2: []
          , spy3: [ 0, 1 ]
          }
      Test.Unit.Assert.equal expected actual

testAlt :: Test.Unit.TestSuite
testAlt =
  Test.Unit.suite "alt (Alt)" do
    Test.Unit.test "should combine two event streams and preserve time topology" do
      actual <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          s2 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            combined :: FRP.Event Int
            combined = s1.event <|> s2.event
          spy1 <- Test.Util.subscribeSpy combined
          s1.push 0
          s2.push 1
          s2.push 2
          s1.push 3
          spy1.received
      Test.Unit.Assert.equal [ 0, 1, 2, 3 ] actual
    Test.Unit.test "obeys associative law: (x <|> y) <|> z = x <|> (y <|> z)" do
      let
        pulses ::
          { s1 :: FRP.SinkEvent Int
          , s2 :: FRP.SinkEvent Int
          , s3 :: FRP.SinkEvent Int
          } ->
          Effect Unit
        pulses { s1, s2, s3 } = do
          s1.push 0
          s2.push 1
          s3.push 2
      left <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          s2 :: FRP.SinkEvent Int <- FRP.sinkEvent
          s3 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            combined :: FRP.Event Int
            combined = (s1.event <|> s2.event) <|> s3.event
          spy1 <- Test.Util.subscribeSpy combined
          pulses { s1, s2, s3 }
          spy1.received
      right <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          s2 :: FRP.SinkEvent Int <- FRP.sinkEvent
          s3 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            combined :: FRP.Event Int
            combined = s1.event <|> (s2.event <|> s3.event)
          spy1 <- Test.Util.subscribeSpy combined
          pulses { s1, s2, s3 }
          spy1.received
      Test.Unit.Assert.equal left right
    Test.Unit.test "distributes map: f <$> (x <|> y) == (f <$> x) <|> (f <$> y)" do
      let
        f :: Int -> Int
        f = (+) 1

        pulses ::
          { s1 :: FRP.SinkEvent Int, s2 :: FRP.SinkEvent Int } ->
          Effect Unit
        pulses { s1, s2 } = do
          s1.push 0
          s2.push 1
      left <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          s2 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            combined :: FRP.Event Int
            combined = f <$> (s1.event <|> s2.event)
          spy1 <- Test.Util.subscribeSpy combined
          pulses { s1, s2 }
          spy1.received
      right <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          s2 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            combined :: FRP.Event Int
            combined = (f <$> s2.event) <|> (f <$> s1.event)
          spy1 <- Test.Util.subscribeSpy combined
          pulses { s1, s2 }
          spy1.received
      Test.Unit.Assert.equal left right
    Test.Unit.test "obeys commutative law: x <|> y = y <|> x" do
      let
        pulses ::
          { s1 :: FRP.SinkEvent Int, s2 :: FRP.SinkEvent Int } ->
          Effect Unit
        pulses { s1, s2 } = do
          s1.push 0
          s2.push 1
      left <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          s2 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            combined :: FRP.Event Int
            combined = s1.event <|> s2.event
          spy1 <- Test.Util.subscribeSpy combined
          pulses { s1, s2 }
          spy1.received
      right <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          s2 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            combined :: FRP.Event Int
            combined = s2.event <|> s1.event
          spy1 <- Test.Util.subscribeSpy combined
          pulses { s1, s2 }
          spy1.received
      Test.Unit.Assert.equal left right

testEmpty :: Test.Unit.TestSuite
testEmpty =
  Test.Unit.suite "empty (Plus)" do
    Test.Unit.test "should never pulse" do
      actual <-
        liftEffect do
          let
            never :: FRP.Event Int
            never = Control.Plus.empty
          spy1 <- Test.Util.subscribeSpy never
          spy1.received
      Test.Unit.Assert.equal [] actual

testMap :: Test.Unit.TestSuite
testMap =
  Test.Unit.suite "map (Functor)" do
    Test.Unit.test "should transform the published values" do
      actual <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            transformed :: FRP.Event Int
            transformed = map (_ + 1) s1.event
          spy1 <- Test.Util.subscribeSpy transformed
          Data.Traversable.for_ [ 0, 1, 2 ] s1.push
          spy1.received
      Test.Unit.Assert.equal [ 1, 2, 3 ] actual
    Test.Unit.test "obeys identity law: map identity = identity" do
      left <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            transformed :: FRP.Event Int
            transformed = map identity s1.event
          spy1 <- Test.Util.subscribeSpy transformed
          Data.Traversable.for_ [ 0, 1, 2 ] s1.push
          spy1.received
      right <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            transformed :: FRP.Event Int
            transformed = identity s1.event
          spy1 <- Test.Util.subscribeSpy transformed
          Data.Traversable.for_ [ 0, 1, 2 ] s1.push
          spy1.received
      Test.Unit.Assert.equal left right
    Test.Unit.test "distributes over compose: map (g <<< f) = map g <<< map f" do
      let
        f :: Int -> Int
        f = (+) 1

        g :: Int -> String
        g = show
      left <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            transformed :: FRP.Event String
            transformed = map (g <<< f) s1.event
          spy1 <- Test.Util.subscribeSpy transformed
          Data.Traversable.for_ [ 0, 1, 2 ] s1.push
          spy1.received
      right <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            transformed :: FRP.Event String
            transformed = (map g <<< map f) s1.event
          spy1 <- Test.Util.subscribeSpy transformed
          Data.Traversable.for_ [ 0, 1, 2 ] s1.push
          spy1.received
      Test.Unit.Assert.equal left right

testFilter :: Test.Unit.TestSuite
testFilter =
  Test.Unit.suite "filter" do
    Test.Unit.test "should remove unwanted pulses" do
      actual <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            predicate :: Int -> Boolean
            predicate x = x `mod` 2 == 0

            filtered :: FRP.Event Int
            filtered = FRP.filter predicate s1.event
          spy1 <- Test.Util.subscribeSpy filtered
          Data.Traversable.for_ (Data.Array.range 0 9) s1.push
          spy1.received
      Test.Unit.Assert.equal [ 0, 2, 4, 6, 8 ] actual
