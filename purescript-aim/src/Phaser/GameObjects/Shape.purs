module Phaser.GameObjects.Shape
  ( Shape
  , setAngle
  , setIsFilled
  , setIsStroked
  , setFillStyle
  , setStrokeStyle
  , toGameObject
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Unsafe.Coerce as Unsafe.Coerce

---------------------------
-- Phaser.GameObjects.Shape
---------------------------
foreign import data Shape :: Type

toGameObject :: Shape -> Phaser.GameObjects.GameObject.GameObject
toGameObject = Unsafe.Coerce.unsafeCoerce

foreign import _setAngle ::
  Effect.Uncurried.EffectFn1
    { shape :: Shape
    , degrees :: Number
    }
    Unit

setAngle :: Shape -> { degrees :: Number } -> Effect Unit
setAngle shape { degrees } =
  Effect.Uncurried.runEffectFn1
    _setAngle
    { shape
    , degrees
    }

foreign import _setIsFilled ::
  Effect.Uncurried.EffectFn1
    { shape :: Shape
    , isFilled :: Boolean
    }
    Unit

setIsFilled :: Shape -> Boolean -> Effect Unit
setIsFilled shape isFilled =
  Effect.Uncurried.runEffectFn1
    _setIsFilled
    { shape
    , isFilled
    }

foreign import _setIsStroked ::
  Effect.Uncurried.EffectFn1
    { shape :: Shape
    , isStroked :: Boolean
    }
    Unit

setIsStroked :: Shape -> Boolean -> Effect Unit
setIsStroked shape isStroked =
  Effect.Uncurried.runEffectFn1
    _setIsStroked
    { shape
    , isStroked
    }

foreign import _setFillStyle ::
  Effect.Uncurried.EffectFn1
    { shape :: Shape
    , color :: Int
    , alpha :: Number
    }
    Unit

setFillStyle ::
  Shape ->
  { color :: Int
  , alpha :: Number
  } ->
  Effect Unit
setFillStyle shape { color, alpha } =
  Effect.Uncurried.runEffectFn1
    _setFillStyle
    { shape
    , color
    , alpha
    }

foreign import _setStrokeStyle ::
  Effect.Uncurried.EffectFn1
    { shape :: Shape
    , lineWidth :: Number
    , color :: Int
    , alpha :: Number
    }
    Unit

setStrokeStyle ::
  Shape ->
  { lineWidth :: Number
  , color :: Int
  , alpha :: Number
  } ->
  Effect Unit
setStrokeStyle shape { lineWidth, color, alpha } =
  Effect.Uncurried.runEffectFn1
    _setStrokeStyle
    { shape
    , lineWidth
    , color
    , alpha
    }
