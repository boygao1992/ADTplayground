module Dropdown where

import Prelude

import Data.Const (Const)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Type.Data.Symbol (SProxy(..))

import Select as S
import Select.Setters as SS

type StateRow =
  ( items :: Array String
  , selection :: Maybe String
  , buttonLabel :: String
  )

type EmbeddedState = S.State StateRow

-- defaultInitialState :: State
-- defaultInitialState =
--   { items: []
--   , selection: Nothing
--   , buttonLabel: ""
--   }

data Action
  = NoOp

type EmbeddedAction = S.Action Action

type Query = Const Void

type EmbeddedQuery = S.Query Query ChildSlots

type Input =
  { items :: Array String
  , buttonLabel :: String
  }

type EmbeddedInput = S.Input StateRow

data Output
  = SelectionChanged { prev :: Maybe String, current :: Maybe String }

type ChildSlots = ()

type EmbeddedComponentM m a
  = H.HalogenM EmbeddedState EmbeddedAction ChildSlots Output m a
type EmbeddedComponent m = H.Component HH.HTML EmbeddedQuery Input Output m
type EmbeddedComponentHTML m = H.ComponentHTML EmbeddedAction ChildSlots m
type EmbeddedComponentRender m = EmbeddedState -> EmbeddedComponentHTML m

type EmbeddedSlot = S.Slot Query ChildSlots Output

type EmbeddedSpec m = S.Spec StateRow Query Action ChildSlots Output m

input :: Input -> EmbeddedInput
input { items, buttonLabel } =
  { inputType: S.Toggle
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: Array.length <<< _.items
  , items
  , selection: Nothing
  , buttonLabel
  }

render :: forall m. MonadAff m => EmbeddedComponentRender m
render { buttonLabel, selection, visibility, items } =
  HH.div_
  [ renderToggle
  , renderContainer
  ]

  where
    renderToggle :: EmbeddedComponentHTML m
    renderToggle =
      HH.button (SS.setToggleProps [])
      [ HH.text $ fromMaybe buttonLabel selection ]


    renderContainer :: EmbeddedComponentHTML m
    renderContainer =
      if visibility == S.On
      then
        HH.div (SS.setContainerProps [])
        $ Array.mapWithIndex renderItem items
      else
        HH.text ""

    renderItem :: Int -> String -> EmbeddedComponentHTML m
    renderItem idx item =
      HH.div ( SS.setItemProps idx [] )
      [ HH.text item ]

handleMessage :: forall m. MonadAff m => S.Message -> EmbeddedComponentM m Unit
handleMessage = case _ of
  S.Selected idx -> do
    prev <- H.gets _.selection
    selection <- H.gets $ (Array.index <@> idx) <<< _.items
    H.modify_ _ { selection = selection, visibility = S.Off }
    H.raise $ SelectionChanged { prev, current: selection }
  _ -> pure unit
