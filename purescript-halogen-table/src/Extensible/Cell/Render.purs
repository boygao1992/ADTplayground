module Extensible.Cell.Render where

import Halogen.HTML as HH
import Effect.Class (class MonadEffect)
import Extensible.Cell.Renderless (ComponentHTML, ComponentRender)
import Extensible.Cell.Setters (setDisplayProps, setGhostElementProps, setInputProps)

renderInput
  :: forall pa cs m. MonadEffect m => { cache :: String, width :: Number } -> ComponentHTML pa cs m
renderInput { cache, width } =
  HH.input (setInputProps { cache, width } [])

renderGhostElement :: forall pa cs m. MonadEffect m => { cache :: String } -> ComponentHTML pa cs m
renderGhostElement { cache } =
  HH.p (setGhostElementProps [])
  [ HH.text cache ]

renderDisplay :: forall pa cs m. MonadEffect m => { value :: String } -> ComponentHTML pa cs m
renderDisplay { value } =
  HH.p (setDisplayProps [])
  [ HH.text value ]

render :: forall pa cs m. MonadEffect m => ComponentRender pa cs m
render { editing, value, cache, width } =
  if editing
  then
    HH.div_
    [ renderInput { cache, width }
    , renderGhostElement { cache }
    ]
  else
    renderDisplay { value }
