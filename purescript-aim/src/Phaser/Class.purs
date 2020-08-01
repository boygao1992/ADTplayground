module Phaser.Class
  ( class EventEmitter
  , toEventEmitter
  , class GameObject
  , toGameObject
  , class Shape
  , toShape
  ) where

import Prelude
import Phaser.Events as Phaser.Events.EventEmitter
import Phaser.GameObjects.Arc as Phaser.GameObjects.Arc
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Phaser.GameObjects.Line as Phaser.GameObjects.Line
import Phaser.GameObjects.Rectangle as Phaser.GameObjects.Rectangle
import Phaser.GameObjects.Shape as Phaser.GameObjects.Shape
import Phaser.Input as Phaser.Input

-----------------------------
-- Phaser.Events.EventEmitter
-----------------------------
class EventEmitter a where
  toEventEmitter :: a -> Phaser.Events.EventEmitter.EventEmitter

instance eventEmitterGameObject :: EventEmitter Phaser.GameObjects.GameObject.GameObject where
  toEventEmitter = Phaser.GameObjects.GameObject.toEventEmitter

instance eventEmitterInputPlugin :: EventEmitter Phaser.Input.InputPlugin where
  toEventEmitter = Phaser.Input.toEventEmitter

--------------------------------
-- Phaser.GameObjects.GameObject
--------------------------------
class GameObject a where
  toGameObject :: a -> Phaser.GameObjects.GameObject.GameObject

instance gameObjectShape :: GameObject Phaser.GameObjects.Shape.Shape where
  toGameObject = Phaser.GameObjects.Shape.toGameObject

instance gameObjectArc :: GameObject Phaser.GameObjects.Arc.Arc where
  toGameObject =
    toGameObject
      <<< Phaser.GameObjects.Arc.toShape

instance gameObjectLine :: GameObject Phaser.GameObjects.Line.Line where
  toGameObject =
    toGameObject
      <<< Phaser.GameObjects.Line.toShape

instance gameObjectRectangle :: GameObject Phaser.GameObjects.Rectangle.Rectangle where
  toGameObject =
    toGameObject
      <<< Phaser.GameObjects.Rectangle.toShape

---------------------------
-- Phaser.GameObjects.Shape
---------------------------
class Shape a where
  toShape :: a -> Phaser.GameObjects.Shape.Shape

instance shapeArc :: Shape Phaser.GameObjects.Arc.Arc where
  toShape = Phaser.GameObjects.Arc.toShape

instance shapeLine :: Shape Phaser.GameObjects.Line.Line where
  toShape = Phaser.GameObjects.Line.toShape

instance shapeRectangle :: Shape Phaser.GameObjects.Rectangle.Rectangle where
  toShape = Phaser.GameObjects.Rectangle.toShape
