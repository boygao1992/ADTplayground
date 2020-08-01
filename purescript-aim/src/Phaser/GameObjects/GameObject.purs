module Phaser.GameObjects.GameObject
  ( GameObject
  , setInteractive_
  , toEventEmitter
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.Events as Phaser.Events.EventEmitter
import Unsafe.Coerce as Unsafe.Coerce

foreign import data GameObject :: Type

toEventEmitter :: GameObject -> Phaser.Events.EventEmitter.EventEmitter
toEventEmitter = Unsafe.Coerce.unsafeCoerce

foreign import _setInteractive_ ::
  Effect.Uncurried.EffectFn1
    GameObject
    Unit

setInteractive_ :: GameObject -> Effect Unit
setInteractive_ gameObject =
  Effect.Uncurried.runEffectFn1
    _setInteractive_
    gameObject
