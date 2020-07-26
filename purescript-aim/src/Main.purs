module Main where

import Prelude
import Data.Function.Uncurried as Data.Function.Uncurried
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried

main :: Effect Unit
main = do
  pointIntersectLine <- buildPointIntersectLine
  newGame
    { width: 800.0
    , height: 600.0
    , scene: [ pointIntersectLine ]
    , type: auto
    }
  where
  buildPointIntersectLine :: Effect Scene
  buildPointIntersectLine =
    buildScene
      { create
      , preload: \_ -> pure unit
      }

  create ::
    SceneContext ->
    Effect Unit
  create context = do
    graphics <- addGraphics context
    line <-
      newLine
        { x: 300.0
        , y: 200.0
        }
        { x: 500.0
        , y: 400.0
        }
    point <-
      newPoint
        { x: 200.0
        , y: 100.0
        }
    inputPlugin <- input context
    onPointerMove inputPlugin \pointer -> do
      pointFromPointer <-
        newPoint
          { x: pointer.x
          , y: pointer.y
          }
      render graphics line pointFromPointer
    render graphics line point

render ::
  Graphics ->
  Line ->
  Point ->
  Effect Unit
render g line point = do
  clear g
  lineStyle g
    { alpha: 1.0
    , color: 0x00ff00
    , lineWidth: 2.0
    }
  strokeLineShape g line
  fillStyle g
    { alpha: 1.0
    , color:
        if pointToLine point line { lineThickness: 5.0 } then
          0xff0000
        else
          0xffff00
    }
  fillPointShape g point { size: 5.0 }

--------------
-- Phaser.Game
--------------
type Config
  = { height :: Number
    , scene :: Array Scene
    , type :: Renderer
    , width :: Number
    }

foreign import data Renderer :: Type

foreign import _newGame ::
  Effect.Uncurried.EffectFn1
    Config
    Unit

newGame ::
  Config ->
  Effect Unit
newGame config = Effect.Uncurried.runEffectFn1 _newGame config

---------------
-- Phaser.Input
---------------
foreign import data InputPlugin :: Type

type Pointer
  = { x :: Number
    , y :: Number
    }

foreign import _onPointerMove ::
  Effect.Uncurried.EffectFn1
    { inputPlugin :: InputPlugin
    , callback :: Effect.Uncurried.EffectFn1 Pointer Unit
    }
    Unit

onPointerMove ::
  InputPlugin ->
  (Pointer -> Effect Unit) ->
  Effect Unit
onPointerMove inputPlugin callback =
  Effect.Uncurried.runEffectFn1
    _onPointerMove
    { inputPlugin
    , callback: Effect.Uncurried.mkEffectFn1 callback
    }

--------------
-- Phaser.Geom
--------------
foreign import data Line :: Type

foreign import data Point :: Type

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

----------------
-- Phaser.Scenes
----------------
foreign import data Scene :: Type

foreign import data SceneContext :: Type

foreign import _buildScene ::
  Effect.Uncurried.EffectFn1
    { preload :: Effect.Uncurried.EffectFn1 SceneContext Unit
    , create :: Effect.Uncurried.EffectFn1 SceneContext Unit
    }
    Scene

buildScene ::
  { preload :: SceneContext -> Effect Unit
  , create :: SceneContext -> Effect Unit
  } ->
  Effect Scene
buildScene { preload, create } =
  Effect.Uncurried.runEffectFn1
    _buildScene
    { preload: Effect.Uncurried.mkEffectFn1 preload
    , create: Effect.Uncurried.mkEffectFn1 create
    }

foreign import _addGraphics ::
  Effect.Uncurried.EffectFn1
    SceneContext
    Graphics

addGraphics :: SceneContext -> Effect Graphics
addGraphics context = Effect.Uncurried.runEffectFn1 _addGraphics context

foreign import _input ::
  Effect.Uncurried.EffectFn1
    SceneContext
    InputPlugin

input :: SceneContext -> Effect InputPlugin
input context = Effect.Uncurried.runEffectFn1 _input context

------------------------------
-- Phaser.GameObjects.Graphics
------------------------------
foreign import data Graphics :: Type

foreign import _clear ::
  Effect.Uncurried.EffectFn1
    Graphics
    Unit

clear :: Graphics -> Effect Unit
clear graphics = Effect.Uncurried.runEffectFn1 _clear graphics

foreign import _fillPointShape ::
  Effect.Uncurried.EffectFn1
    { graphics :: Graphics
    , point :: Point
    , size :: Number
    }
    Unit

fillPointShape ::
  Graphics ->
  Point ->
  { size :: Number } ->
  Effect Unit
fillPointShape graphics point { size } =
  Effect.Uncurried.runEffectFn1
    _fillPointShape
    { graphics
    , point
    , size
    }

foreign import _fillStyle ::
  Effect.Uncurried.EffectFn1
    { graphics :: Graphics
    , color :: Int
    , alpha :: Number
    }
    Unit

fillStyle ::
  Graphics ->
  { alpha :: Number
  , color :: Int
  } ->
  Effect Unit
fillStyle graphics { alpha, color } =
  Effect.Uncurried.runEffectFn1
    _fillStyle
    { graphics
    , color
    , alpha
    }

foreign import _lineStyle ::
  Effect.Uncurried.EffectFn1
    { graphics :: Graphics
    , lineWidth :: Number
    , color :: Int
    , alpha :: Number
    }
    Unit

lineStyle ::
  Graphics ->
  { alpha :: Number
  , color :: Int
  , lineWidth :: Number
  } ->
  Effect Unit
lineStyle graphics { alpha, color, lineWidth } =
  Effect.Uncurried.runEffectFn1
    _lineStyle
    { graphics
    , lineWidth
    , color
    , alpha
    }

foreign import _strokeLineShape ::
  Effect.Uncurried.EffectFn1
    { graphics :: Graphics
    , line :: Line
    }
    Unit

strokeLineShape ::
  Graphics ->
  Line ->
  Effect Unit
strokeLineShape graphics line =
  Effect.Uncurried.runEffectFn1
    _strokeLineShape
    { graphics
    , line
    }

--------------
-- Phaser.Core
--------------
foreign import auto :: Renderer
