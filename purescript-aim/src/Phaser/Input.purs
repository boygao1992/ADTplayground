module Phaser.Input
  ( InputPlugin
  , Pointer
  , point
  , onPointerMove
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Unsafe.Coerce as Unsafe.Coerce

---------------------------
-- Phaser.Input.InputPlugin
---------------------------
foreign import data InputPlugin :: Type

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

-----------------------
-- Phaser.Input.Pointer
-----------------------
foreign import data Pointer :: Type

point :: Pointer -> { x :: Number, y :: Number }
point = Unsafe.Coerce.unsafeCoerce
