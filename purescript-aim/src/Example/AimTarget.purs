module Example.AimTarget
  ( scene
  ) where

import Prelude
import Data.Foldable as Data.Foldable
import Data.Int as Data.Int
import Effect (Effect)
import Effect.Console as Effect.Console
import FRP as FRP
import Phaser.Events as Phaser.Event.EventEmitter
import Phaser.GameObjects as Phaser.GameObjects
import Phaser.GameObjects.Arc as Phaser.GameObjects.Arc
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Phaser.GameObjects.GameObjectFactory as Phaser.GameObjects.GameObjectFactory
import Phaser.GameObjects.Rectangle as Phaser.GameObjects.Rectangle
import Phaser.GameObjects.Shape as Phaser.GameObjects.Shape
import Phaser.Input as Phaser.Input
import Phaser.Scene as Phaser.Scene

scene :: Effect Phaser.Scene.Scene
scene =
  Phaser.Scene.buildScene
    { create
    , preload: \_ -> pure unit
    }
  where
  create ::
    Phaser.Scene.SceneContext ->
    Effect Unit
  create context = do
    gameObjectFactory <- Phaser.Scene.add context
    Data.Foldable.for_ [ 1, 4 ] \idx -> do
      let
        degrees :: Number
        degrees = 30.0 * (Data.Int.toNumber idx)
      addLine gameObjectFactory { degrees, fillColor: 0x91aec2 }
    Data.Foldable.for_ [ 2, 5 ] \idx -> do
      let
        degrees :: Number
        degrees = 30.0 * (Data.Int.toNumber idx)
      addLine gameObjectFactory { degrees, fillColor: 0x94b187 }
    Data.Foldable.for_ [ 0, 3 ] \idx -> do
      let
        degrees :: Number
        degrees = 30.0 * (Data.Int.toNumber idx)
      addLine gameObjectFactory { degrees, fillColor: 0xc05049 }
    addCircle gameObjectFactory

  addCircle ::
    Phaser.GameObjects.GameObjectFactory ->
    Effect Unit
  addCircle gameObjectFactory = do
    circle <-
      Phaser.GameObjects.GameObjectFactory.circle
        gameObjectFactory
        { x: 400.0
        , y: 400.0
        , radius: 400.0
        , fillColor: 0x00ff00
        , fillAlpha: 1.0
        }
    Phaser.GameObjects.Shape.setStrokeStyle
      (Phaser.GameObjects.Arc.toShape circle)
      { lineWidth: 2.0
      , color: 0xc05049
      , alpha: 1.0
      }
    Phaser.GameObjects.Shape.setIsFilled
      (Phaser.GameObjects.Arc.toShape circle)
      false
    pure unit

  addLine ::
    Phaser.GameObjects.GameObjectFactory ->
    { degrees :: Number
    , fillColor :: Int
    } ->
    Effect Unit
  addLine gameObjectFactory { degrees, fillColor } = do
    line <-
      Phaser.GameObjects.GameObjectFactory.rectangle
        gameObjectFactory
        { x: 400.0
        , y: 400.0
        , width: 800.0
        , height: 2.0
        , fillColor
        , fillAlpha: 1.0
        }
    Phaser.GameObjects.Shape.setAngle
      (Phaser.GameObjects.Rectangle.toShape line)
      { degrees }
    Phaser.GameObjects.GameObject.setInteractive_ (Phaser.GameObjects.Shape.toGameObject (Phaser.GameObjects.Rectangle.toShape line))
    pointerOverE <- onPointerOverE (Phaser.GameObjects.GameObject.toEventEmitter (Phaser.GameObjects.Shape.toGameObject (Phaser.GameObjects.Rectangle.toShape line)))
    void
      $ FRP.subscribe pointerOverE \payload -> do
          Effect.Console.logShow { x: payload.localX, y: payload.localY }

  onPointerOverE ::
    Phaser.Event.EventEmitter.EventEmitter ->
    Effect
      ( FRP.Event
          { pointer :: Phaser.Input.Pointer
          , localX :: Number
          , localY :: Number
          }
      )
  onPointerOverE eventSource = do
    sink <- FRP.sinkEvent
    Phaser.Event.EventEmitter.onPointerOver eventSource \pointer -> do
      sink.push pointer
    pure sink.event
