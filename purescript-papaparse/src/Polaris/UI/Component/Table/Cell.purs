module Polaris.UI.Component.Table.Cell where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.HTML.HTMLElement as WHE
import Web.UIEvent.KeyboardEvent as KE
import Polaris.UI.Block.DataTable as DataTable
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)

import Halogen.HTML.CSS as HC
import CSS (absolute, display, fromString, height, inlineBlock, key, nil, padding, position, px, textWhitespace, top, whitespaceNoWrap, width) as CSS
import CSS.Overflow (hidden, overflow) as CSS
import CSS.Visibility (visibility, visibilityHidden) as CSS

type State =
  { editing :: Boolean
  , value :: String
  , cache :: String
  , width :: Number
  }

defaultInitialState :: State
defaultInitialState =
  { editing: false
  , value: ""
  , cache: ""
  , width: 0.0
  }

data Action
  = Edit
  | OnValueInput String
  | OnKeyDown KE.KeyboardEvent
  | OnBlur

data Query a
  = GetValue (String -> a)

type Input = String

type Output = Void

type ChildSlots = ()

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \value ->
      defaultInitialState
        { value = value
        }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

_input = H.RefLabel "input" :: H.RefLabel
_ghost = H.RefLabel "auto-size-input-ghost" :: H.RefLabel

render :: forall m. MonadAff m => ComponentRender m
render { editing, value, cache, width } =
  DataTable.cell_
  [ if editing
    then
      HH.div_
      [ HH.input  [ HP.value cache
                  , HE.onKeyDown $ Just <<< OnKeyDown
                  , HE.onValueInput $ Just <<< OnValueInput
                  , HE.onBlur $ Just <<< const OnBlur
                  , HP.ref _input
                  , HC.style do
                      CSS.key (CSS.fromString "border") "none"
                      CSS.padding (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0) (CSS.px 0.0)
                      CSS.width (CSS.px width)
                  ]
      , HH.p  [ HC.style do
                  CSS.display CSS.inlineBlock
                  CSS.height CSS.nil
                  CSS.overflow CSS.hidden
                  CSS.position CSS.absolute
                  CSS.top CSS.nil
                  CSS.visibility CSS.visibilityHidden
                  CSS.textWhitespace CSS.whitespaceNoWrap
              , HP.ref _ghost
              ]
        [ HH.text cache ]
      ]
    else
      HH.p [ HE.onClick <<< const <<< Just $ Edit
           ]
      [ HH.text value ]
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  Edit -> do
    H.modify_ \st -> st { editing = true, cache = st.value }
    focusInput
    updateInputWidth

  OnValueInput str -> do
    H.modify_ _ { cache = str }
    updateInputWidth

  OnKeyDown ke -> do
    H.liftEffect $ log $ KE.key ke
    case KE.key ke of
      "Enter" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        H.modify_ \st -> st { editing = false, value = st.cache }
        blurInput

      "Escape" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        H.modify_ \st -> st { editing = false }
        blurInput

      _ -> do
        pure unit

  OnBlur -> do
    H.modify_ \st -> st { editing = false }

  where
    updateInputWidth :: ComponentM m Unit
    updateInputWidth =
      void $ runMaybeT do
        ghostEl <- MaybeT $ H.getHTMLElementRef _ghost
        textWidth
          <- H.lift <<< H.liftEffect
          $ map _.width
          $ WHE.getBoundingClientRect
          $ ghostEl
        H.lift $ H.modify_ _ { width = textWidth }

    focusInput :: ComponentM m Unit
    focusInput =
      void $ runMaybeT do
        el <- MaybeT $ H.getHTMLElementRef _input
        H.lift $ H.liftEffect $ WHE.focus el

    blurInput :: ComponentM m Unit
    blurInput =
      void $ runMaybeT do
        el <- MaybeT $ H.getHTMLElementRef _input
        H.lift $ H.liftEffect $ WHE.blur el

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetValue reply -> do
    value <- H.gets _.value
    pure $ Just <<< reply $ value

