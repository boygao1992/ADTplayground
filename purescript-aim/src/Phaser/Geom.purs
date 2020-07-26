module Phaser.Geom
  ( Line
  , Point
  , newLine
  , newPoint
  , pointToLine
  ) where

import Data.Function.Uncurried as Data.Function.Uncurried
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried

-------------------------
-- Phaser.Geom.Intersects
-------------------------
foreign import _pointToLine ::
  Data.Function.Uncurried.Fn3
    Point
    Line
    Number
    Boolean

pointToLine ::
  Point ->
  Line ->
  { lineThickness :: Number } ->
  Boolean
pointToLine point line { lineThickness } =
  Data.Function.Uncurried.runFn3
    _pointToLine
    point
    line
    lineThickness

-------------------
-- Phaser.Geom.Line
-------------------
foreign import data Line :: Type

foreign import _newLine ::
  Effect.Uncurried.EffectFn1
    { startX :: Number
    , startY :: Number
    , endX :: Number
    , endY :: Number
    }
    Line

newLine ::
  { x :: Number
  , y :: Number
  } ->
  { x :: Number
  , y :: Number
  } ->
  Effect Line
newLine start end =
  Effect.Uncurried.runEffectFn1
    _newLine
    { startX: start.x
    , startY: start.y
    , endX: end.x
    , endY: end.y
    }

--------------------
-- Phaser.Geom.Point
--------------------
foreign import data Point :: Type

foreign import _newPoint ::
  Effect.Uncurried.EffectFn1
    { x :: Number
    , y :: Number
    }
    Point

newPoint ::
  { x :: Number
  , y :: Number
  } ->
  Effect Point
newPoint payload = Effect.Uncurried.runEffectFn1 _newPoint payload
