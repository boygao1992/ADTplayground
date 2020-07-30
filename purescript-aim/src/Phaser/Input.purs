module Phaser.Input
  ( InputPlugin
  , Pointer
  , point
  , onPointerMove
  ) where

import Prelude
import Data.Function.Uncurried as Data.Function.Uncurried
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried

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

foreign import _point ::
  Data.Function.Uncurried.Fn1
    Pointer
    { x :: Number
    , y :: Number
    }

point :: Pointer -> { x :: Number, y :: Number }
point pointer =
  Data.Function.Uncurried.runFn1
    _point
    pointer
