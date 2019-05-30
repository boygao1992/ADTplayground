module Magento.Import.UI.Container.Categories.Item where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.Read (read)
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Util (debug, debugShow)
import Magento.Import.Data.Categories (Category(..))
import Magento.Import.UI.Component.Checkbox as Checkbox
import Magento.Import.UI.Component.TextField as TextField
import Magento.Import.UI.Data.Validity as Validity
import Ocelot.Block.Card as Card
import Ocelot.Block.Checkbox (checkbox_) as Ocelot
import Ocelot.Block.Input as Input

type State =
  { category :: Category
  , validity :: Validity.Validity
  , selected :: Boolean
  }

initialState :: State
initialState =
  { category: Category []
  , validity: Validity.NotValidated
  , selected: false
  }

data Action
  = HandleCheckbox Checkbox.Output
  | HandleTextField TextField.Output

data Query a
  = GetCategory (Category -> a)
  | GetValidity (Maybe Validity.Validity -> a)

type Input =
  { category :: Category
  , validity :: Validity.Validity
  , selected :: Boolean
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
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

render :: forall m. MonadAff m => ComponentRender m
render st =
  Card.innerCard
  [ HP.classes $ HH.ClassName <$>
    [ Validity.borderColor $ if st.selected
        then Just st.validity
        else Nothing
    , Validity.backgroundColor $ if st.selected
        then Just st.validity
        else Nothing
    , "border-3"
    , "rounded"
    , "pl-3"
    ]
  ]
  [ Input.inputGroup_
    [ HH.slot TextField._textField unit TextField.component
        { render: renderTextField
        , initialState: show st.category
        }
        (Just <<< HandleTextField)
    , HH.span
      [ HP.classes $ HH.ClassName <$>
        [ "bg-transparent"
        , "order-start"
        , "cursor-pointer"
        , "text-grey-70"
        ]
      ]
      [ HH.slot Checkbox._checkbox unit Checkbox.component
        { render: renderCheckbox
        , initialState: st.selected
        }
        (Just <<< HandleCheckbox)
      ]
    ]
  ]
  where
    renderTextField :: TextField.ComponentRender m
    renderTextField str =
      Input.input
      (TextField.setProps str
       [ HP.classes $ HH.ClassName <$>
         [ "mr-3" ]
          -- "bg-white border-t-2 border-b-2 font-light cc-blue-88 border-grey-80 disabled:bg-grey-95 disabled:text-grey-70 focus:no-outline py-2 border-l-2 border-r-2 w-full px-3 focus:border-blue-88 !focus:!disabled:hover:border-grey-70"
       ]
      )
    renderCheckbox :: Checkbox.ComponentRender m
    renderCheckbox b =
      Ocelot.checkbox_
      ( Checkbox.setProps b
        []
      )
      []

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  HandleTextField (TextField.Updated str) -> do
    for_ (read str) \c -> do
      H.modify_ _ { category = c }
      debugShow c
      H.modify_ _ { validity = Validity.Valid } -- TODO simulate validation
      H.raise StatusUpdate

  HandleCheckbox (Checkbox.Checked checked) -> do
    H.modify_ _ { selected = checked }
    if checked
      then debug "checked"
      else debug "unchecked"
    H.raise StatusUpdate

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetCategory reply -> do
    st <- H.get
    if st.validity == Validity.Valid && st.selected == true
      then pure $ Just $ reply st.category
      else pure Nothing

  GetValidity reply -> do
    ({ validity, selected }) <- H.get
    pure $ Just $ reply $ if selected
      then Just validity
      else Nothing
