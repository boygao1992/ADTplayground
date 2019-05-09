module Example where

import Prelude

import Halogen.Storybook (Stories, proxy)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Data.Tuple (Tuple(..))
import Example.Basic.Component as Basic

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "basic" $ proxy Basic.component
  ]
