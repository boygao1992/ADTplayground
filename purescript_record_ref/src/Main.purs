module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Type.Data.Symbol (SProxy(..))
import RecordRef as RecordRef

main :: Effect Unit
main = do
  rec <- RecordRef.new { a: { b: "wenbo" } }
  RecordRef.pathWrite
    (SProxy :: SProxy "")
    {a : { b : "webot" } }
    rec
  RecordRef.pathModify_
    (SProxy :: SProxy "a.b")
    (const "robot")
    rec
  b <- RecordRef.pathRead (SProxy :: SProxy "a") rec
  logShow b
