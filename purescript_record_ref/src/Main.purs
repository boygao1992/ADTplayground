module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import RecordRef (PCons, PNil, PLProxy(..))
import RecordRef as RecordRef

main :: Effect Unit
main = do
  rec <- RecordRef.new { a: { b: "wenbo" } }
  RecordRef.pathWrite
    (PLProxy :: PLProxy PNil)
    {a : { b : "webot"}}
    rec
  RecordRef.pathModify_
    (PLProxy :: PLProxy (PCons "a" (PCons "b" PNil)))
    (const "robot")
    rec
  b <- RecordRef.pathRead (PLProxy :: PLProxy (PCons "a" PNil)) rec
  logShow b
