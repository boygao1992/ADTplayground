module Phaser.GameObjects.Graphics
  ( Graphics
  , clear
  , fillPointShape
  , fillStyle
  , lineStyle
  , strokeLineShape
  , toGameObject
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Phaser.Geom as Phaser.Geom
import Unsafe.Coerce as Unsafe.Coerce

------------------------------
-- Phaser.GameObjects.Graphics
------------------------------
foreign import data Graphics :: Type

toGameObject :: Graphics -> Phaser.GameObjects.GameObject.GameObject
toGameObject = Unsafe.Coerce.unsafeCoerce

foreign import _clear ::
  Effect.Uncurried.EffectFn1
    Graphics
    Unit

clear :: Graphics -> Effect Unit
clear graphics = Effect.Uncurried.runEffectFn1 _clear graphics

foreign import _fillPointShape ::
  Effect.Uncurried.EffectFn1
    { graphics :: Graphics
    , point :: Phaser.Geom.Point
    , size :: Number
    }
    Unit

fillPointShape ::
  Graphics ->
  Phaser.Geom.Point ->
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
    , line :: Phaser.Geom.Line
    }
    Unit

strokeLineShape ::
  Graphics ->
  Phaser.Geom.Line ->
  Effect Unit
strokeLineShape graphics line =
  Effect.Uncurried.runEffectFn1
    _strokeLineShape
    { graphics
    , line
    }
