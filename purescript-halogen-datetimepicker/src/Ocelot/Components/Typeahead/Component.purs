module Ocelot.Components.Typeahead.Component where

import Prelude

import Renderless.State
import Control.Alternative (class Plus, empty)
import Data.Array ((!!), difference, filter, length, sort, (:))
import Data.Fuzzy (Fuzzy(..))
import Data.Fuzzy as Fuzz
import Data.Maybe (Maybe(..), maybe)
import Data.Rational ((%))
import Data.Time.Duration (Milliseconds)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.RemoteData (RemoteData(..))
import Renderless.State (getState, modifyState, modifyState_, modifyStore_)
import Select as S
import Type.Data.Symbol (SProxy(..))


type Slot f item id = H.Slot (Query f item) (Output f item) id

type Component f item m = H.Component HH.HTML (Query f item) (Input f item m) (Output f item) m
type ComponentHTML f item m = H.ComponentHTML (Action f item m) (ChildSlots f item) m
type ComponentRender f item m = State f item m -> ComponentHTML f item m
type ComponentM f item m a = H.HalogenM (StateStore f item m) (Action f item m) (ChildSlots f item) (Output f item) m a

type StateRow f item m =
  ( items :: RemoteData String (Array item) -- NOTE pst.items, Parent(Typeahead)
  , selected :: f item
  , insertable :: Insertable item
  , keepOpen :: Boolean
  , itemToObject :: item -> Object String
  , ops :: Operations f item
  , debounceTime :: Maybe Milliseconds
  , async :: Maybe (String -> m (RemoteData String (Array item)))

  , fuzzyItems :: Array (Fuzzy item) -- NOTE cst.items, Child(Select)
  )

type State f item m = Record (StateRow f item m)

type StateStore f item m = Store (State f item m) (ComponentHTML f item m)

type Input f item m =
  { items :: RemoteData String (Array item)
  , insertable :: Insertable item
  , keepOpen :: Boolean
  , itemToObject :: item -> Object String
  , debounceTime :: Maybe Milliseconds
  , async :: Maybe (String -> m (RemoteData String (Array item)))

  , renderDropdown :: CompositeComponentRender f item m
  }

data Action f item m
  = PassingOutput (Output f item)
  | ReceiveRender (Input f item m)

data EmbeddedAction (f :: Type -> Type) item (m :: Type -> Type)
  = Remove item
  | RemoveAll
  -- | Receive CompositeInput
-- NOTE internal actions, moved to Util functions
  -- | Synchronize a
-- NOTE not in use
-- NOTE deprecated
  -- | Search String
  -- | TriggerFocus a
  -- | HandleSelect (Select.Message (Query pq f item m) (Fuzzy item)) a
  -- | AndThen (Query pq f item m Unit) (Query pq f item m Unit) a
  -- | Raise (pq Unit) a

data Query f item a
  = GetSelected (f item -> a)
  | ReplaceSelected (f item) a
  | ReplaceSelectedBy (Array item -> f item) a
  | ReplaceItems (RemoteData String (Array item)) a
  | Reset a

data Output (f :: Type -> Type) item
  = Searched String
  | Selected item
  | SelectionChanged SelectionCause (f item)
  -- | Emit (pq Unit)

type ChildSlots f item =
  ( select :: S.Slot (Query f item) EmbeddedChildSlots (Output f item) Unit
  )
_select = SProxy :: SProxy "select"


type CompositeState f item m = S.State (StateRow f item m)
type CompositeAction f item m = S.Action (EmbeddedAction f item m)
type CompositeQuery f item = S.Query (Query f item) EmbeddedChildSlots
type CompositeInput f item m = S.Input (StateRow f item m)
type EmbeddedChildSlots = () -- NOTE no extension

type Spec f item m = S.Spec (StateRow f item m) (Query f item) (EmbeddedAction f item m) EmbeddedChildSlots (Output f item) m
type CompositeComponent f item m = H.Component HH.HTML (CompositeQuery f item) (CompositeInput f item m) (Output f item) m
type CompositeComponentHTML f item m = H.ComponentHTML (CompositeAction f item m) EmbeddedChildSlots m
type CompositeComponentRender f item m = (CompositeState f item m) -> CompositeComponentHTML f item m
type CompositeComponentM f item m a = H.HalogenM (CompositeState f item m) (CompositeAction f item m) EmbeddedChildSlots (Output f item) m a

-------
-- Data

data SelectionCause
  = RemovalQuery
  | ReplacementQuery
  | ResetQuery
  | SelectionMessage
derive instance eqSelectionCause :: Eq SelectionCause

type Operations f item =
  { runSelect  :: item -> f item -> f item
  , runRemove  :: item -> f item -> f item
  , runFilter  :: Array item -> f item -> Array item
  }

data Insertable item
  = NotInsertable
  | Insertable (String -> item)


------------
-- Container

-- component
--   :: forall item m
--   . MonadAff m
--   => Component item m
-- component = H.mkComponent
--   { initialState: \({ renderDropdown, selectedItem, items }) ->
--       store (renderAdapter renderDropdown) { selectedItem, items }
--   , render: extract
--   , eval: H.mkEval H.defaultEval
--       { handleAction = handleAction
--       , handleQuery = handleQuery
--       }
--   }

-- renderAdapter
--   :: forall item m
--   . MonadAff m
--   => CompositeComponentRender item m
--   -> ComponentRender item m
-- renderAdapter renderDropdown state =
--   HH.slot _select unit (S.component $ spec renderDropdown)
--     (embeddedInput state)
--     (Just <<< PassingOutput)

-- spec
--   :: forall item m
--   . MonadAff m
--   => CompositeComponentRender item m
--   -> Spec item m
-- spec embeddedRender =
--   S.defaultSpec
--   { render = embeddedRender
--   -- , handleAction = embeddedHandleAction
--   , handleQuery = embeddedHandleQuery
--   , handleMessage = embeddedHandleMessage
--   }

-- NOTE configure Select
-- embeddedInput :: forall item. State item -> CompositeInput item
-- embeddedInput { selectedItem, items } =
--   { inputType: S.Toggle
--   , search: Nothing
--   , debounceTime: Nothing
--   , getItemCount: Array.length <<< _.items

--   , selectedItem
--   , items
--   }

-- NOTE re-raise output messages from the embedded component
-- NOTE update Dropdown render function if it relies on external state
-- handleAction :: forall item m. MonadAff m => Action item m -> ComponentM item m Unit
-- handleAction = case _ of
--   PassingOutput output ->
--     H.raise output

--   ReceiveRender { renderDropdown } -> do
--     modifyStore_ (renderAdapter renderDropdown) identity

-- NOTE passing query to the embedded component
-- handleQuery :: forall item m a. Query item a -> ComponentM item m (Maybe a)
-- handleQuery = case _ of
--   SetItems items a -> Just a <$ do
--     H.query _select unit (S.Query $ H.tell $ SetItems items)

--   SetSelection item a -> Just a <$ do
--     H.query _select unit (S.Query $ H.tell $ SetSelection item)

------------------
-- Embedded > Util

getNewItems
  :: forall f item m
  . MonadAff m
  => Eq item
  => CompositeState f item m
  -> RemoteData String (Array (Fuzzy item))
getNewItems st =
  sort
  <<< applyF
  <<< applyI
  <<< fuzzyItems
  <$> (map (flip st.ops.runFilter st.selected) st.items)
  where
    matcher :: item -> Fuzzy item
    matcher = Fuzz.match true st.itemToObject st.search

    fuzzyItems :: Array item -> Array (Fuzzy item)
    fuzzyItems = map matcher

    applyI :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyI = applyInsertable matcher st.insertable st.search

    applyF :: Array (Fuzzy item) -> Array (Fuzzy item)
    applyF = filter (\(Fuzzy { ratio }) -> ratio > (2 % 3))

applyInsertable
  :: forall item
  . (item -> Fuzzy item)
  -> Insertable item
  -> String
  -> Array (Fuzzy item)
  -> Array (Fuzzy item)
applyInsertable _ _ "" items = items
applyInsertable match insertable text items = case insertable of
  NotInsertable -> items
  Insertable mkItem | length (filter isExactMatch items) > 0 -> items
                    | otherwise -> (match $ mkItem text) : items
  where
    isExactMatch (Fuzzy { distance }) = distance == Fuzz.Distance 0 0 0 0 0 0

synchronize
  :: forall f item m
  . Eq item
  => MonadAff m
  => CompositeComponentM f item m Unit
synchronize = do
  st <- H.get
  case getNewItems st of
    Success items -> do
      H.modify_ _ { fuzzyItems = items }
    Failure err -> do
      H.modify_
        _ { visibility = S.Off
          , fuzzyItems = []
          }
    NotAsked -> do
      H.modify_
        _ { visibility = S.Off
          , fuzzyItems = []
          }
    Loading -> do
      H.modify_ _ { fuzzyItems = [] }

replaceSelected
  :: forall f item m
  . Eq item
  => MonadAff m
  => f item
  -> CompositeComponentM f item m Unit
replaceSelected selected = do
  st <- H.modify _ { selected = selected }
  H.raise $ SelectionChanged ReplacementQuery st.selected
  synchronize

--------------------------
-- Embedded > handleAction

embeddedHandleAction
  :: forall f item m
  . Eq item
  => Plus f
  => MonadAff m
  => EmbeddedAction f item m
  -> CompositeComponentM f item m Unit
embeddedHandleAction = case _ of
  Remove item -> do
    st <- H.modify \st -> st { selected = st.ops.runRemove item st.selected }
    H.raise $ SelectionChanged RemovalQuery st.selected
    synchronize
  RemoveAll -> do
    st <- H.modify \st -> st { selected = empty :: f item }
    H.raise $ SelectionChanged RemovalQuery st.selected
    synchronize

  -- Receive input a -> do
  --   H.modify_ $ updateStore input.render identity
  --   pure a

-------------------------
-- Embedded > handleQuery

embeddedHandleQuery
  :: forall f item m a
  . Plus f
  => Eq item
  => MonadAff m
  => Query f item a
  -> CompositeComponentM f item m (Maybe a)
embeddedHandleQuery = case _ of
  GetSelected reply -> do
    selected  <- H.gets _.selected
    pure $ Just $ reply selected

  ReplaceSelected selected a -> Just a <$ do
    replaceSelected selected

  ReplaceSelectedBy f a -> Just a <$ do
    items  <- H.gets _.items
    case items of
      Success items' -> replaceSelected (f items')
      _ -> pure unit

  ReplaceItems items a -> Just a <$ do
    H.modify_ _ { items = items }
    synchronize

  Reset a -> Just a <$ do
    st <- H.modify _ { selected = empty :: f item, items = NotAsked }
    H.raise $ SelectionChanged ResetQuery st.selected
    synchronize

---------------------------
-- Embedded > handleMessage

embeddedHandleMessage
  :: forall f item m
  . Eq item
  => MonadAff m
  => S.Message
  -> CompositeComponentM f item m Unit
embeddedHandleMessage = case _ of
  S.Selected idx -> do
    -- (Fuzzy { original: item })
    fuzzyItems <- H.gets _.fuzzyItems
    case fuzzyItems !! idx of
      Nothing -> pure unit
      Just (Fuzzy { original: item }) -> do
        st <- H.modify \st -> st { selected = st.ops.runSelect item st.selected }
        when st.keepOpen do
          H.modify_ _ { visibility = S.Off }
        -- if st.keepOpen
        --   then pure Nothing
        --   else H.query unit $ Select.setVisibility Select.Off
        H.raise $ SelectionChanged SelectionMessage st.selected
        H.raise $ Selected item
        synchronize

  -- Perform a new search, fetching data if Async.
  S.Searched text -> do
    H.modify_ _ { search = text }

    async <- H.gets _.async
    case async of
      Nothing -> pure unit
      Just fetchItems -> do
        H.modify_ _ { items = Loading }
        synchronize
        newItems <- H.lift $ fetchItems text
        H.modify_ _ { items = newItems }

    H.raise $ Searched text
    synchronize

  _ -> pure unit


--------------------
-- Embedded > render
