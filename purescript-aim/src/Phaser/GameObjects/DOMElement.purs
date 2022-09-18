module Phaser.GameObjects.DOMElement
  ( DOMElement
  , setElement
  , toGameObject
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.GameObjects.GameObject as Phaser.GameObjects.GameObject
import Unsafe.Coerce as Unsafe.Coerce
import Web.DOM as Web.DOM

--------------------------------
-- Phaser.GameObjects.DOMElement
--------------------------------
foreign import data DOMElement :: Type

toGameObject :: DOMElement -> Phaser.GameObjects.GameObject.GameObject
toGameObject = Unsafe.Coerce.unsafeCoerce

foreign import _setElement ::
  Effect.Uncurried.EffectFn1
    { domElement :: DOMElement
    , element :: Web.DOM.Element
    }
    Unit

setElement ::
  DOMElement ->
  Web.DOM.Element ->
  Effect Unit
setElement domElement element =
  Effect.Uncurried.runEffectFn1
    _setElement
    { domElement
    , element
    }
