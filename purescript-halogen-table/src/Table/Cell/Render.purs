module Table.Cell.Render where

import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Table.Cell.Setters as CS

import Table.Cell.Renderless (ComponentHTML, ComponentRender)

renderInput
  :: forall m. MonadAff m => { cache :: String, width :: Number } -> ComponentHTML m
renderInput { cache, width } =
  HH.input (CS.setInputProps { cache, width } [])

renderGhostElement :: forall m. MonadAff m => { cache :: String } -> ComponentHTML m
renderGhostElement { cache } =
  HH.p (CS.setGhostElementProps [])
  [ HH.text cache ]

renderDisplay :: forall m. MonadAff m => { value :: String } -> ComponentHTML m
renderDisplay { value } =
  HH.p (CS.setDisplayProps [])
  [ HH.text value ]

render :: forall m. MonadAff m => ComponentRender m
render { editing, value, cache, width } =
  if editing
  then
    HH.div_
    [ renderInput { cache, width }
    , renderGhostElement { cache }
    ]
  else
    renderDisplay { value }
