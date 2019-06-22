module Adhoc.Cell.Text.Render where

import Prelude

import Data.Array as Array
import Data.Monoid (guard) as Monoid
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Adhoc.Cell.Text.Renderless (ComponentRender)
import Adhoc.Cell.Text.Setters (setDisplayProps, setGhostElementProps, setInputProps, setItemProps, setMenuProps)

defaultRender :: forall m. MonadAff m => ComponentRender m
defaultRender { editing, value, cache, width, isOpen, highlightedIndex, candidates, displayMessage } =
  if editing
  then
    HH.div []
    [ HH.input $ setInputProps { cache, width } []
    , if displayMessage
      then
        HH.p [ HP.class_ $ HH.ClassName "text-red-600" ]
        [ HH.text "not an valid input" ]
      else HH.text ""
    , if isOpen
      then
        HH.ul ( setMenuProps
                [ HP.class_ $ HH.ClassName "shadow-md bg-white"
                ])
        $ Array.mapWithIndex renderItem candidates
      else
        HH.text ""
    , HH.p (setGhostElementProps [])
      [ HH.text cache ]
    ]
  else
    HH.p (setDisplayProps [])
    [ HH.text value ]

  where
    renderItem index item =
      HH.li ( setItemProps
              { index
              , isSelected: index == highlightedIndex
              }
              [ HP.class_ $ HH.ClassName $ "py-1 px-3 cursor-pointer"
                  <> Monoid.guard (index == highlightedIndex) " bg-blue-300"
              ])
      [ HH.text item ]
