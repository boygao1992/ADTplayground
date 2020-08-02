module Example.AimTarget
  ( scene
  ) where

import Prelude
import Data.Foldable as Data.Foldable
import Data.Int as Data.Int
import Effect (Effect)
import Effect.Console as Effect.Console
import FRP as FRP
import Phaser.Class.Events as Phaser.Class.Events
import Phaser.Class.GameObjects.GameObject as Phaser.Class.GameObjects.GameObject
import Phaser.Class.GameObjects.Shape as Phaser.Class.GameObjects.Shape
import Phaser.GameObjects as Phaser.GameObjects
import Phaser.GameObjects.GameObjectFactory as Phaser.GameObjects.GameObjectFactory
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
    addTarget gameObjectFactory

  addTarget ::
    Phaser.GameObjects.GameObjectFactory ->
    Effect Unit
  addTarget gameObjectFactory = do
    let
      blue :: Int
      blue = 0x91aec2

      green :: Int
      green = 0x94b187

      red :: Int
      red = 0xc05049
    Data.Foldable.for_ [ 1, 4 ] \idx -> do
      let
        degrees :: Number
        degrees = 30.0 * (Data.Int.toNumber idx)
      addLine gameObjectFactory { degrees, fillColor: blue }
    Data.Foldable.for_ [ 2, 5 ] \idx -> do
      let
        degrees :: Number
        degrees = 30.0 * (Data.Int.toNumber idx)
      addLine gameObjectFactory { degrees, fillColor: green }
    Data.Foldable.for_ [ 0, 3 ] \idx -> do
      let
        degrees :: Number
        degrees = 30.0 * (Data.Int.toNumber idx)
      addLine gameObjectFactory { degrees, fillColor: red }
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
        }
    Phaser.Class.GameObjects.Shape.setStrokeStyle
      circle
      { lineWidth: 4.0
      , color: 0xc05049
      , alpha: 1.0
      }
    Phaser.Class.GameObjects.Shape.setIsFilled circle false
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
        , height: 4.0
        }
    Phaser.Class.GameObjects.Shape.setFillStyle
      line
      { color: fillColor
      , alpha: 1.0
      }
    Phaser.Class.GameObjects.Shape.setAngle
      line
      { degrees }
    Phaser.Class.GameObjects.GameObject.setInteractive_ line
    pointerOverE <- onPointerOverE line
    pointerOutE <- onPointerOutE line
    void
      $ FRP.subscribe pointerOverE \payload -> do
          Effect.Console.logShow { degrees, event: "pointerOver", x: payload.localX, y: payload.localY }
          Phaser.Class.GameObjects.Shape.setStrokeStyle
            line
            { lineWidth: 1.0
            , color: 0x000000
            , alpha: 1.0
            }
    void
      $ FRP.subscribe pointerOutE \payload -> do
          Effect.Console.logShow { degrees, event: "pointerOut" }
          Phaser.Class.GameObjects.Shape.setIsStroked line false

  onPointerMoveE ::
    forall a.
    Phaser.Class.Events.EventEmitter a =>
    a ->
    Effect (FRP.Event Phaser.Input.Pointer)
  onPointerMoveE eventSource = do
    sink <- FRP.sinkEvent
    Phaser.Class.Events.onPointerMove eventSource \payload -> do
      sink.push payload.pointer
    pure sink.event

  onPointerOverE ::
    forall a.
    Phaser.Class.Events.EventEmitter a =>
    a ->
    Effect
      ( FRP.Event
          { pointer :: Phaser.Input.Pointer
          , localX :: Number
          , localY :: Number
          }
      )
  onPointerOverE eventSource = do
    sink <- FRP.sinkEvent
    Phaser.Class.Events.onPointerOver eventSource \payload -> do
      sink.push payload
    pure sink.event

  onPointerOutE ::
    forall a.
    Phaser.Class.Events.EventEmitter a =>
    a ->
    Effect (FRP.Event Phaser.Input.Pointer)
  onPointerOutE eventSource = do
    sink <- FRP.sinkEvent
    Phaser.Class.Events.onPointerOut eventSource \pointer -> do
      sink.push pointer
    pure sink.event
