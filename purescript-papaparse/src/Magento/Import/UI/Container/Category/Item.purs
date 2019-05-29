module Magento.Import.UI.Container.Category.Item where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.Read (read)
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
-- import Halogen.Query.EventSource as HES
import Halogen.Util (debug, debugShow)
import Magento.Import.Data.Categories (Category(..))
import Magento.Import.UI.Component.Checkbox as Checkbox
import Magento.Import.UI.Component.TextField as TextField
import Ocelot.Block.Checkbox (checkbox_) as Ocelot
import Ocelot.Block.Input as Input

data Validity
  = NotValidated
  | Validating
  | Invalid
  | Valid
derive instance genericValidity :: Generic Validity _
derive instance eqValidity :: Eq Validity
derive instance ordValidity :: Ord Validity
instance showValidity :: Show Validity where
  show = genericShow

type State =
  { category :: Category
  , validity :: Validity
  , selected :: Boolean
  }

initialState :: State
initialState =
  { category: Category []
  , validity: NotValidated
  , selected: false
  }

data Action
  = HandleCheckbox Checkbox.Output
  | HandleTextField TextField.Output

data Query a
  = GetCategory (Category -> a)

type Input =
  { category :: Category
  , validity :: Boolean
  }

data Output
  = StatusUpdate

type ChildSlots =
  ( checkbox :: Checkbox.Slot Unit
  , textField :: TextField.Slot Unit
  )

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({category, validity}) ->
      initialState
      { category = category
      , validity = if validity then Valid else Invalid
      , selected = if validity then true else false
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render st =
  Input.inputGroup_
  [ HH.slot TextField._textField unit TextField.component
      { render: TextField.render
      , initialState: show st.category
      }
      (Just <<< HandleTextField)
  , Input.addonLeft_
    [ HH.slot Checkbox._checkbox unit Checkbox.component
      { render: renderCheckbox
      , initialState: st.selected
      }
      (Just <<< HandleCheckbox)
    ]
  ]
  where
    renderCheckbox :: Checkbox.ComponentRender m
    renderCheckbox b =
      Ocelot.checkbox_
      ( Checkbox.setProps b
        [ HP.disabled $ st.validity /= Valid ]
      )
      []

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  HandleTextField (TextField.Updated str) -> do
    for_ (read str) \c -> do
      H.modify_ _ { category = c }
      debugShow c
      H.modify_ _ { validity = Valid } -- TODO simulate validation

  HandleCheckbox output -> case output of
    Checkbox.Checked -> do
      H.modify_ _ { selected = true }
      debug "checked"
    Checkbox.Unchecked -> do
      H.modify_ _ { selected = false }
      debug "unchecked"

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetCategory reply -> do
    st <- H.get
    if st.validity == Valid && st.selected == true
      then pure $ Just $ reply st.category
      else pure Nothing
