module Phaser.Game
  ( Config
  , Renderer
  , auto
  , newGame
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as Effect.Uncurried
import Phaser.Scene as Phaser.Scene

type Config
  = { height :: Number
    , scene :: Array Phaser.Scene.Scene
    , type :: Renderer
    , width :: Number
    }

foreign import data Renderer :: Type

foreign import auto :: Renderer

foreign import _newGame ::
  Effect.Uncurried.EffectFn1
    Config
    Unit

newGame :: Config -> Effect Unit
newGame config = Effect.Uncurried.runEffectFn1 _newGame config
