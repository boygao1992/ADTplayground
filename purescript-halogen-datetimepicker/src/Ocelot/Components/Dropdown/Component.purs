module Ocelot.Components.Dropdown.Component where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Data.Array ((!!))
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.ItemContainer as IC
import Ocelot.HTML.Properties (css)
import Renderless.State (Store, extract, modifyStore_, store)
import Select as S
import Select.Setters as SS
import Type.Data.Symbol (SProxy(..))


type Slot item id = H.Slot (Query item) (Output item) id

type Component item m = H.Component HH.HTML (Query item) (Input item m) (Output item) m
type ComponentHTML item m = H.ComponentHTML (Action item m) (ChildSlots item) m
type ComponentRender item m = State item -> ComponentHTML item m
type ComponentM item m a = H.HalogenM (StateStore item m) (Action item m) (ChildSlots item) (Output item) m a

type StateRow item =
  ( selectedItem :: Maybe item
  , items :: Array item
  )

type State item = Record (StateRow item)

type StateStore item m = Store (State item) (ComponentHTML item m)

type Input item m =
  { selectedItem :: Maybe item
  , items :: Array item

  , renderDropdown :: CompositeComponentRender item m
  }

data Action item m
  = PassingOutput (Output item)
  | ReceiveRender (Input item m)

data EmbeddedAction
-- | Receive CompositeInput

data Query item a
  = SetItems (Array item) a
  | SetSelection (Maybe item) a

data Output item
  = Selected item
  | VisibilityChanged S.Visibility
  -- | Emit (o Unit)

type ChildSlots item =
  ( select :: S.Slot (Query item) EmbeddedChildSlots (Output item) Unit
  )
_select = SProxy :: SProxy "select"


type CompositeState item = S.State (StateRow item)
type CompositeAction = S.Action EmbeddedAction
type CompositeQuery item = S.Query (Query item) EmbeddedChildSlots
type CompositeInput item = S.Input (StateRow item)
type EmbeddedChildSlots = () -- NOTE no extension

type Spec item m = S.Spec (StateRow item) (Query item) EmbeddedAction EmbeddedChildSlots (Output item) m
type CompositeComponent item m = H.Component HH.HTML (CompositeQuery item) (CompositeInput item) (Output item) m
type CompositeComponentHTML m = H.ComponentHTML CompositeAction EmbeddedChildSlots m
type CompositeComponentRender item m = (CompositeState item) -> CompositeComponentHTML m
type CompositeComponentM item m a = H.HalogenM (CompositeState item) CompositeAction EmbeddedChildSlots (Output item) m a


------------
-- Container

component
  :: forall item m
  . MonadAff m
  => Component item m
component = H.mkComponent
  { initialState: \({ renderDropdown, selectedItem, items }) ->
      store (renderAdapter renderDropdown) { selectedItem, items }
  , render: extract
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

renderAdapter
  :: ∀ item m
  . MonadAff m
  => CompositeComponentRender item m
  -> ComponentRender item m
renderAdapter renderDropdown state =
  HH.slot _select unit (S.component $ spec renderDropdown)
    (embeddedInput state)
    (Just <<< PassingOutput)

spec
  :: forall item m
  . MonadAff m
  => CompositeComponentRender item m
  -> Spec item m
spec embeddedRender =
  S.defaultSpec
  { render = embeddedRender
  -- , handleAction = embeddedHandleAction
  , handleQuery = embeddedHandleQuery
  , handleMessage = embeddedHandleMessage
  }

-- NOTE configure Select
embeddedInput :: forall item. State item -> CompositeInput item
embeddedInput { selectedItem, items } =
  { inputType: S.Toggle
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: Array.length <<< _.items

  , selectedItem
  , items
  }

-- NOTE re-raise output messages from the embedded component
-- NOTE update Dropdown render function if it relies on external state
handleAction :: forall item m. MonadAff m => Action item m -> ComponentM item m Unit
handleAction = case _ of
  PassingOutput output ->
    H.raise output

  ReceiveRender { renderDropdown } -> do
    modifyStore_ (renderAdapter renderDropdown) identity

-- NOTE passing query to the embedded component
handleQuery :: forall item m a. Query item a -> ComponentM item m (Maybe a)
handleQuery = case _ of
  SetItems items a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ SetItems items)

  SetSelection item a -> Just a <$ do
    H.query _select unit (S.Query $ H.tell $ SetSelection item)

--------------------------
-- Embedded > handleAction

  -- Receive input a -> do
  --   H.modify_ $ updateStore input.render identity
  --   pure a

-------------------------
-- Embedded > handleQuery

embeddedHandleQuery
  :: forall item m a. MonadAff m => Query item a -> CompositeComponentM item m (Maybe a)
embeddedHandleQuery = case _ of
  SetItems items a -> Just a <$ do
    H.modify_ _ { items = items }

  SetSelection item a -> Just a <$ do
    H.modify_ _ { selectedItem = item }

---------------------------
-- Embedded > handleMessage

embeddedHandleMessage
  :: forall item m. MonadAff m => S.Message -> CompositeComponentM item m Unit
embeddedHandleMessage = case _ of
  S.Selected idx -> do
    items <- H.gets _.items
    case items !! idx of
      Nothing -> pure unit
      Just item -> do
        H.modify_
          _ { visibility = S.Off
            , selectedItem = Just item
            }
        H.raise $ Selected item

  S.VisibilityChanged vis -> H.raise (VisibilityChanged vis)

  _ -> pure unit

--------------------
-- Embedded > render

type ButtonBlock p i
  = Array (HH.IProp HTMLbutton i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i

defDropdown
  :: ∀ item m
  . Eq item
  => (∀ p i. ButtonBlock p i)
  -> Array (HP.IProp HTMLbutton CompositeAction)
  -> (item -> String)
  -> String
  -> CompositeComponentRender item m
defDropdown button props toString label st =
  HH.div [ css "relative" ] [ toggle, menu ]

  where
    toggle =
      IC.dropdownButton
        button
        (SS.setToggleProps props)
        [ HH.text $ maybe label toString st.selectedItem ]

    menu = HH.div
      [ HP.classes containerClasses ]
      [ IC.dropdownContainer
        []
        (HH.text <<< toString)
        ((==) st.selectedItem <<< Just)
        st.items
        st.highlightedIndex
      ]

    containerClasses = case st.visibility of
      S.Off -> [ HH.ClassName "invisible" ]
      S.On -> []

