module Table.Autocomplete where

import Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Extensible.NSelect as S
import Extensible.Cell as Cell

type State =
  { value :: String
  , items :: Array String
  , filteredItems :: Array String
  }

data Action
  = HandleDropdown (S.Output ())

type Query = Const Void

type Input = Unit
type Output = Void

type ChildSlots =
  ( dropdown :: S.Slot () Unit
  )

_dropdown = SProxy :: SProxy "dropdown"

type SelectSlots =
  ( cell :: Cell.Slot (S.ActionRow ()) Unit
  )

_cell = SProxy :: SProxy "cell"

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

items :: Array String
items =
  [ "purescript-css-validate"
  , "purescript-halogen-color-picker"
  , "purescript-halogen-day-picker"
  , "purescript-halogen-nselect"
  , "purescript-halogen-storybook"
  , "purescript-halogen-transition"
  , "purescript-jest"
  , "purescript-svg-parser"
  , "purescript-svgo"
  , "svgen"
  ]

defaultInitialState :: State
defaultInitialState =
  { value: ""
  , items
  , filteredItems: []
  }

render :: forall m. MonadAff m => ComponentRender m
render state =
  HH.slot _dropdown unit S.component
    { render: renderSelect
    , itemCount: Array.length state.filteredItems
    } $ Just <<< HandleDropdown
  where
    renderSelect :: S.ComponentRender () SelectSlots m
    renderSelect { isOpen, highlightedIndex } =
      HH.div ( S.setRootProps [] )
      $ join
      [ pure $
        HH.input  ( S.setInputProps
                    [ HH.attr (HH.AttrName "style") "width: 20rem"
                    , HP.value state.value
                    ])
      , guard isOpen $>
        HH.div ( S.setMenuProps
                 [ HP.class_ $ HH.ClassName "shadow-md bg-white" ])
        [ HH.ul [ HH.attr (HH.AttrName "style")  "max-height: 10rem;" ]
          (Array.mapWithIndex renderItem state.filteredItems)
        ]
      ]
      where
        renderItem index item =
          HH.li ( S.setItemProps {index, isSelected: index == highlightedIndex }
                  [ HP.class_ $ HH.ClassName $ "py-1 px-3 cursor-pointer"
                  <> Monoid.guard (index == highlightedIndex) " bg-blue-300"
                  ])
          [ HH.text item ]



component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: const defaultInitialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction (HandleDropdown msg) = case msg of
  S.Selected index -> do
    state <- H.get
    for_ (Array.index state.filteredItems index) \item ->
      H.modify_ $ _ { value = item }
    void $ H.query _dropdown unit $ H.tell S.Close
  S.InputValueChanged value -> do
    H.modify_ $ \state -> state
      { value = value
      , filteredItems = Array.filter (\s -> String.contains (String.Pattern value) s) state.items
      }
    void $ H.query _dropdown unit $ H.tell S.Open

  S.VisibilityChanged _ -> pure unit

  S.RaiseUnhandled _ -> pure unit
