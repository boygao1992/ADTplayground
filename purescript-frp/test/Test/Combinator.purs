module Test.Combinator (suite) where

import Prelude
import Data.Array as Data.Array
import Data.Traversable as Data.Traversable
import Effect.Class (liftEffect)
import FRP as FRP
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert
import Test.Util as Test.Util

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Combinator" do
    testAccum
    testFoldl
    testSampleBy
    testStep

testAccum :: Test.Unit.TestSuite
testAccum =
  Test.Unit.suite "accum" do
    Test.Unit.test "should fold over a event stream with a reducer and keep latest result in a behavior" do
      actual <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            reducer :: String -> Int -> String
            reducer state x = state <> show x
          accumulatorB :: FRP.Behavior String <-
            FRP.runNow do
              FRP.accum reducer "" s1.event
          Data.Traversable.for_ (Data.Array.range 0 3) s1.push
          FRP.runNow do
            FRP.pull accumulatorB
      Test.Unit.Assert.equal "0123" actual

testFoldl :: Test.Unit.TestSuite
testFoldl =
  Test.Unit.suite "foldl" do
    Test.Unit.test "should fold over a event stream with a reducer and report results in another event stream" do
      actual <-
        liftEffect do
          s1 :: FRP.SinkEvent Int <- FRP.sinkEvent
          let
            reducer :: String -> Int -> String
            reducer state x = state <> show x
          accumulatorE :: FRP.Event String <-
            FRP.runNow do
              FRP.foldl reducer "" s1.event
          spy1 <- Test.Util.subscribeSpy accumulatorE
          Data.Traversable.for_ (Data.Array.range 0 3) s1.push
          spy1.received
      Test.Unit.Assert.equal [ "0", "01", "012", "0123" ] actual

testSampleBy :: Test.Unit.TestSuite
testSampleBy =
  Test.Unit.suite "sampleBy" do
    Test.Unit.test "should combine a behavior and a event stream into a event stream with a binary function" do
      actual <-
        liftEffect do
          s1 :: FRP.SinkEvent String <- FRP.sinkEvent
          s2 :: FRP.SinkEvent Int <- FRP.sinkEvent
          b <-
            FRP.runNow do
              FRP.step "" s1.event
          let
            f :: String -> Int -> String
            f prefix x = prefix <> show x

            combined :: FRP.Event String
            combined = FRP.sampleBy f b s2.event
          spy1 <- Test.Util.subscribeSpy combined
          s2.push 0
          s1.push "+"
          s2.push 1
          s2.push 2
          s1.push "-"
          s2.push 3
          spy1.received
      Test.Unit.Assert.equal [ "0", "+1", "+2", "-3" ] actual

testStep :: Test.Unit.TestSuite
testStep =
  Test.Unit.suite "Step" do
    Test.Unit.test "should keep the latest value from a event stream in a behavior" do
      actual <-
        liftEffect do
          s1 :: FRP.SinkEvent String <- FRP.sinkEvent
          b <-
            FRP.runNow do
              FRP.step "" s1.event
          s1.push "+"
          s1.push "-"
          FRP.runNow do
            FRP.pull b
      Test.Unit.Assert.equal "-" actual
    Test.Unit.test "should present the initial value if no pulse received yet" do
      actual <-
        liftEffect do
          s1 :: FRP.SinkEvent String <- FRP.sinkEvent
          b <-
            FRP.runNow do
              FRP.step "" s1.event
          FRP.runNow do
            FRP.pull b
      Test.Unit.Assert.equal "" actual
