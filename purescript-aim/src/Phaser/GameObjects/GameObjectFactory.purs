module Phaser.GameObjects.GameObjectFactory
  ( GameObjectFactory
  , arc
  , container
  , group
  , line
  ) where

import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.Arc as Phaser.GameObjects.Arc
import Phaser.GameObjects.Container as Phaser.GameObjects.Container
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Phaser.GameObjects.Group as Phaser.GameObjects.Group
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

foreign import _container ::
  Effect.Uncurried.EffectFn1
    { gameObjectFactory :: GameObjectFactory
    , x :: Number
    , y :: Number
    , children :: Array Phaser.GameObjects.GameObject.GameObject
    }
    Phaser.GameObjects.Container.Container

container ::
  GameObjectFactory ->
  { x :: Number
  , y :: Number
  } ->
  Array Phaser.GameObjects.GameObject.GameObject ->
  Effect Phaser.GameObjects.Container.Container
container gameObjectFactory { x, y } children =
  Effect.Uncurried.runEffectFn1
    _container
    { gameObjectFactory
    , x
    , y
    , children
    }

foreign import _group ::
  Effect.Uncurried.EffectFn1
    { gameObjectFactory :: GameObjectFactory
    , children :: Array Phaser.GameObjects.GameObject.GameObject
    }
    Phaser.GameObjects.Group.Group

group ::
  GameObjectFactory ->
  Array Phaser.GameObjects.GameObject.GameObject ->
  Effect Phaser.GameObjects.Group.Group
group gameObjectFactory children =
  Effect.Uncurried.runEffectFn1
    _group
    { gameObjectFactory
    , children
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