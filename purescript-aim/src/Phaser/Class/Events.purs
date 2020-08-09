module Phaser.Class.Events
  ( onPointerDown
  , onPointerMove
  , onPointerOut
  , onPointerOver
  , class EventEmitter
  , toEventEmitter
  ) where

import Prelude

import Effect (Effect)
import Phaser.Events as Phaser.Events
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Phaser.GameObjects.Rectangle as Phaser.GameObjects.Rectangle
import Phaser.GameObjects.Shape as Phaser.GameObjects.Shape
import Phaser.Input as Phaser.Input
import Phaser.Input.Pointer as Phaser.Input.Pointer

-----------------------------
-- Phaser.Events
-----------------------------
class EventEmitter a where
  toEventEmitter :: a -> Phaser.Events.EventEmitter

instance eventEmitterEventEmitter :: EventEmitter Phaser.Events.EventEmitter where
  toEventEmitter = identity

instance eventEmitterGameObject :: EventEmitter Phaser.GameObjects.GameObject.GameObject where
  toEventEmitter = Phaser.GameObjects.GameObject.toEventEmitter

instance eventEmitterShape :: EventEmitter Phaser.GameObjects.Shape.Shape where
  toEventEmitter =
    toEventEmitter
      <<< Phaser.GameObjects.Shape.toGameObject

instance eventEmitterRectangle :: EventEmitter Phaser.GameObjects.Rectangle.Rectangle where
  toEventEmitter =
    toEventEmitter
      <<< Phaser.GameObjects.Rectangle.toShape

instance eventEmitterInputPlugin :: EventEmitter Phaser.Input.InputPlugin where
  toEventEmitter = Phaser.Input.toEventEmitter

onPointerDown ::
  forall a.
  EventEmitter a =>
  a ->
  (Phaser.Input.Pointer.Pointer -> Effect Unit) ->
  Effect Unit
onPointerDown a callback = Phaser.Events.onPointerDown (toEventEmitter a) callback

onPointerMove ::
  forall a.
  EventEmitter a =>
  a ->
  ( { pointer :: Phaser.Input.Pointer.Pointer
    , localX :: Number
    , localY :: Number
    } ->
    Effect Unit
  ) ->
  Effect Unit
onPointerMove a callback = Phaser.Events.onPointerMove (toEventEmitter a) callback

onPointerOut ::
  forall a.
  EventEmitter a =>
  a ->
  (Phaser.Input.Pointer.Pointer -> Effect Unit) ->
  Effect Unit
onPointerOut a callback = Phaser.Events.onPointerOut (toEventEmitter a) callback

onPointerOver ::
  forall a.
  EventEmitter a =>
  a ->
  ( { pointer :: Phaser.Input.Pointer.Pointer
    , localX :: Number
    , localY :: Number
    } ->
    Effect Unit
  ) ->
  Effect Unit
onPointerOver a callback = Phaser.Events.onPointerOver (toEventEmitter a) callback
