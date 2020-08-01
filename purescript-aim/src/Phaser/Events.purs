module Phaser.Events
  ( EventEmitter
  , onPointerMove
  , onPointerOver
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.Input.Pointer as Phaser.Input.Pointer

-----------------------------
-- Phaser.Events.EventEmitter
-----------------------------
foreign import data EventEmitter :: Type

foreign import _onPointerMove ::
  Effect.Uncurried.EffectFn1
    { eventEmitter :: EventEmitter
    , callback :: Effect.Uncurried.EffectFn1 Phaser.Input.Pointer.Pointer Unit
    }
    Unit

onPointerMove ::
  EventEmitter ->
  (Phaser.Input.Pointer.Pointer -> Effect Unit) ->
  Effect Unit
onPointerMove eventEmitter callback =
  Effect.Uncurried.runEffectFn1
    _onPointerMove
    { eventEmitter
    , callback: Effect.Uncurried.mkEffectFn1 callback
    }

foreign import _onPointerOver ::
  Effect.Uncurried.EffectFn1
    { eventEmitter :: EventEmitter
    , callback ::
        Effect.Uncurried.EffectFn3
          Phaser.Input.Pointer.Pointer
          Number
          Number
          Unit
    }
    Unit

onPointerOver ::
  EventEmitter ->
  ( { pointer :: Phaser.Input.Pointer.Pointer
    , localX :: Number
    , localY :: Number
    } ->
    Effect Unit
  ) ->
  Effect Unit
onPointerOver eventEmitter callback =
  Effect.Uncurried.runEffectFn1
    _onPointerOver
    { eventEmitter
    , callback:
        Effect.Uncurried.mkEffectFn3 \pointer localX localY ->
          callback { pointer, localX, localY }
    }
