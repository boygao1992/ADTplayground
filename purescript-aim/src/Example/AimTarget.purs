module Example.AimTarget
  ( scene
  ) where

import Prelude
import Data.Array as Data.Array
import Data.Foldable as Data.Foldable
import Data.Int as Data.Int
import Effect (Effect)
import FRP as FRP
import Phaser.GameObjects as Phaser.GameObjects
import Phaser.GameObjects.Arc as Phaser.GameObjects.Arc
import Phaser.GameObjects.GameObjectFactory as Phaser.GameObjects.GameObjectFactory
import Phaser.GameObjects.Line as Phaser.GameObjects.Line
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
    Data.Foldable.for_ (Data.Array.range 0 6) \idx -> do
      let
        degrees :: Number
        degrees = 30.0 * (Data.Int.toNumber idx)
      addLine gameObjectFactory { degrees }
    addArc gameObjectFactory

  addArc ::
    Phaser.GameObjects.GameObjectFactory ->
    Effect Unit
  addArc gameObjectFactory = do
    arc <-
      Phaser.GameObjects.GameObjectFactory.arc
        gameObjectFactory
        { x: 400.0
        , y: 400.0
        , radius: 400.0
        , startAngle: -180.0
        , endAngle: 180.0
        , anticlockwise: false
        , fillColor: 0x00ff00
        , fillAlpha: 1.0
        }
    Phaser.GameObjects.Shape.setStrokeStyle
      (Phaser.GameObjects.Arc.toShape arc)
      { lineWidth: 2.0
      , color: 0x00ff00
      , alpha: 1.0
      }
    Phaser.GameObjects.Shape.setIsFilled
      (Phaser.GameObjects.Arc.toShape arc)
      false
    pure unit

  addLine ::
    Phaser.GameObjects.GameObjectFactory ->
    { degrees :: Number } ->
    Effect Unit
  addLine gameObjectFactory degrees = do
    line <-
      Phaser.GameObjects.GameObjectFactory.line
        gameObjectFactory
        { position: { x: 400.0, y: 400.0 }
        , start: { x: 0.0, y: 0.0 }
        , end: { x: 800.0, y: 0.0 }
        , strokeColor: 0x00ff00
        , strokeAlpha: 1.0
        }
    Phaser.GameObjects.Line.setLineWidth line 1.0
    Phaser.GameObjects.Shape.setAngle
      (Phaser.GameObjects.Line.toShape line)
      degrees

  onPointerMoveE ::
    Phaser.Input.InputPlugin ->
    Effect (FRP.Event Phaser.Input.Pointer)
  onPointerMoveE inputPlugin = do
    sink <- FRP.sinkEvent
    Phaser.Input.onPointerMove inputPlugin \pointer -> do
      sink.push pointer
    pure sink.event
