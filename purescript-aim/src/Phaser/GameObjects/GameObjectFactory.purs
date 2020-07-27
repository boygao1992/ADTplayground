module Phaser.GameObjects.GameObjectFactory
  ( GameObjectFactory
  , arc
  , line
  ) where

import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.Arc as Phaser.GameObjects.Arc
import Phaser.GameObjects.Line as Phaser.GameObjects.Line

---------------------------------------
-- Phaser.GameObjects.GameObjectFactory
---------------------------------------
foreign import data GameObjectFactory :: Type

foreign import _arc ::
  Effect.Uncurried.EffectFn1
    { gameObjectFactory :: GameObjectFactory
    , x :: Number
    , y :: Number
    , radius :: Number
    , startAngle :: Number
    , endAngle :: Number
    , anticlockwise :: Boolean
    , fillColor :: Int
    , fillAlpha :: Number
    }
    Phaser.GameObjects.Arc.Arc

arc ::
  GameObjectFactory ->
  { x :: Number
  , y :: Number
  , radius :: Number
  , startAngle :: Number
  , endAngle :: Number
  , anticlockwise :: Boolean
  , fillColor :: Int
  , fillAlpha :: Number
  } ->
  Effect Phaser.GameObjects.Arc.Arc
arc gameObjectFactory { x, y, radius, startAngle, endAngle, anticlockwise, fillColor, fillAlpha } =
  Effect.Uncurried.runEffectFn1
    _arc
    { gameObjectFactory
    , x
    , y
    , radius
    , startAngle
    , endAngle
    , anticlockwise
    , fillColor
    , fillAlpha
    }

foreign import _line ::
  Effect.Uncurried.EffectFn1
    { gameObjectFactory :: GameObjectFactory
    , x :: Number
    , y :: Number
    , x1 :: Number
    , y1 :: Number
    , x2 :: Number
    , y2 :: Number
    , strokeColor :: Int
    , strokeAlpha :: Number
    }
    Phaser.GameObjects.Line.Line

line ::
  GameObjectFactory ->
  { position :: { x :: Number, y :: Number }
  , start :: { x :: Number, y :: Number }
  , end :: { x :: Number, y :: Number }
  , strokeColor :: Int
  , strokeAlpha :: Number
  } ->
  Effect Phaser.GameObjects.Line.Line
line gameObjectFactory { position, start, end, strokeColor, strokeAlpha } =
  Effect.Uncurried.runEffectFn1
    _line
    { gameObjectFactory
    , x: position.x
    , y: position.y
    , x1: start.x
    , y1: start.y
    , x2: end.x
    , y2: end.y
    , strokeColor
    , strokeAlpha
    }
