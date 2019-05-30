module Magento.Import.UI.Container.Categories.Add where


import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.String.Read (read)
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
-- import Halogen.Query.EventSource as HES
-- import Halogen.Util (debug, debugShow)
import Magento.Import.Data.Categories (Category)
import Magento.Import.UI.Component.Button as Button
import Magento.Import.UI.Component.TextField as TextField
import Ocelot.Block.Input as Input

type State = Unit

initialState :: State
initialState = unit

data Action
  = HandleButton Button.Output

type Query = Const Void

type Input = Unit

data Output
  = Added Category
  | EmptyInput

type ChildSlots =
  ( button :: Button.Slot Unit
  , textField :: TextField.Slot Unit
  )

type ComponentM m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

render :: forall m. MonadAff m => ComponentRender m
render _ =
  Input.inputGroup_
  [ Input.addonLeft_
    [ HH.slot Button._button unit Button.component
      { render: Button.simpleRender } (Just <<< HandleButton)
    ]
  -- TODO add validation logic and share the augmented component with Categories.Item
  , HH.slot TextField._textField unit TextField.component
      { render: TextField.simpleRender
      , initialState: TextField.defaultInitialState
      }
      (const Nothing)
  ]

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  HandleButton Button.Pressed -> do
    ms <- H.query TextField._textField unit (H.request TextField.Get)
    for_ (read =<< ms) \s -> do
      if s == mempty
        then H.raise EmptyInput
        else H.raise $ Added s
