module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FRP as FRP
import Phaser.Game as Phaser.Game
import Phaser.GameObjects.Graphics as Phaser.GameObjects
import Phaser.GameObjects.Graphics as Phaser.GameObjects.Graphics
import Phaser.Geom as Phaser.Geom
import Phaser.Input as Phaser.Input
import Phaser.Scene as Phaser.Scene

main :: Effect Unit
main = do
  pointIntersectLine <- buildPointIntersectLine
  Phaser.Game.newGame
    { width: 800.0
    , height: 600.0
    , scene: [ pointIntersectLine ]
    , type: Phaser.Game.auto
    }
  where
  buildPointIntersectLine :: Effect Phaser.Scene.Scene
  buildPointIntersectLine =
    Phaser.Scene.buildScene
      { create
      , preload: \_ -> pure unit
      }

  create ::
    Phaser.Scene.SceneContext ->
    Effect Unit
  create context = do
    gameObjectFactory <- Phaser.Scene.add context
    graphics <- Phaser.Scene.graphics gameObjectFactory
    line <-
      Phaser.Geom.newLine
        { x: 300.0
        , y: 200.0
        }
        { x: 500.0
        , y: 400.0
        }
    inputPlugin <- Phaser.Scene.input context
    pointerE <- onPointerMoveE inputPlugin
    void
      $ FRP.subscribe pointerE \pointer -> do
          point <- Phaser.Geom.newPoint $ Phaser.Input.point pointer
          render graphics line (Just point)
    render graphics line Nothing

  onPointerMoveE ::
    Phaser.Input.InputPlugin ->
    Effect (FRP.Event Phaser.Input.Pointer)
  onPointerMoveE inputPlugin = do
    sink <- FRP.sinkEvent
    Phaser.Input.onPointerMove inputPlugin \pointer -> do
      sink.push pointer
    pure sink.event

render ::
  Phaser.GameObjects.Graphics ->
  Phaser.Geom.Line ->
  Maybe Phaser.Geom.Point ->
  Effect Unit
render g' line' mPoint = do
  Phaser.GameObjects.Graphics.clear g'
  renderLine g' line'
  case mPoint of
    Nothing -> pure unit
    Just point -> renderPoint g' line' point
  where
  renderLine ::
    Phaser.GameObjects.Graphics ->
    Phaser.Geom.Line ->
    Effect Unit
  renderLine g line = do
    Phaser.GameObjects.Graphics.lineStyle g
      { alpha: 1.0
      , color: 0x00ff00
      , lineWidth: 2.0
      }
    Phaser.GameObjects.Graphics.strokeLineShape g line

  renderPoint ::
    Phaser.GameObjects.Graphics ->
    Phaser.Geom.Line ->
    Phaser.Geom.Point ->
    Effect Unit
  renderPoint g line point = do
    Phaser.GameObjects.Graphics.fillStyle g
      { alpha: 1.0
      , color:
          if Phaser.Geom.pointToLine point line { lineThickness: 5.0 } then
            0xff0000
          else
            0xffff00
      }
    Phaser.GameObjects.Graphics.fillPointShape g point { size: 5.0 }
