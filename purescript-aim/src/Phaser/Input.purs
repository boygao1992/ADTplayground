module Phaser.Input
  ( InputPlugin
  , toEventEmitter
  , module Phaser.Input.Pointer
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.Events as Phaser.Events.EventEmitter
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Phaser.Input.Pointer (Pointer) as Phaser.Input.Pointer
import Unsafe.Coerce as Unsafe.Coerce

---------------------------
-- Phaser.Input.InputPlugin
---------------------------
foreign import data InputPlugin :: Type

toEventEmitter :: InputPlugin -> Phaser.Events.EventEmitter.EventEmitter
toEventEmitter = Unsafe.Coerce.unsafeCoerce

foreign import _setHitAreaRectangle ::
  Effect.Uncurried.EffectFn1
    { inputPlugin :: InputPlugin
    , gameObject :: Phaser.GameObjects.GameObject.GameObject
    , x :: Number
    , y :: Number
    , width :: Number
    , height :: Number
    }
    Unit

setHitAreaRectangle ::
  InputPlugin ->
  Phaser.GameObjects.GameObject.GameObject ->
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  } ->
  Effect Unit
setHitAreaRectangle inputPlugin gameObject { x, y, width, height } =
  Effect.Uncurried.runEffectFn1
    _setHitAreaRectangle
    { inputPlugin
    , gameObject
    , x
    , y
    , width
    , height
    }
