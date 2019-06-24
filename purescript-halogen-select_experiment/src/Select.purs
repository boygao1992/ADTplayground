-- | This module exposes a component that can be used to build accessible selection
-- | user interfaces. You are responsible for providing all rendering, with the help
-- | of the `Select.Setters` module, but this component provides the relevant
-- | behaviors for dropdowns, autocompletes, typeaheads, keyboard-navigable calendars,
-- | and other selection UIs.
module Select where

import Prelude

import Control.Monad.Free (liftF)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
-- import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.ChildQuery (ChildQueryBox)
import Prim.Row as Row
import Record.Builder as Builder
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

data Action action
  = Search String
  | Highlight Target
  | Select Target (Maybe ME.MouseEvent)
  | ToggleClick ME.MouseEvent
  | Focus Boolean
  | Key KE.KeyboardEvent
  | PreventClick ME.MouseEvent
  | SetVisibility Visibility
  | Initialize (Maybe action)
  | Action action
  -- | OnMouseDownContainer ME.MouseEvent
  -- | OnMouseUpContainer ME.MouseEvent
  -- | OnBlur

type Action' = Action Void

-----
-- QUERIES

data Query query slots a
  = Send (ChildQueryBox slots (Maybe a))
  | Query (query a)

type Query' = Query (Const Void) ()

-----
-- Output

data Output
  = Searched String
  | Selected Int
  | VisibilityChanged Visibility

-----
-- HELPER TYPES

-- | The component slot type for easy use in a parent component
type Slot query slots msg = H.Slot (Query query slots) msg

-- | The component slot type when there is no extension
type Slot' = Slot (Const Void) () Void

-- | Represents a way to navigate on `Highlight` events: to the previous
-- | item, next item, or the item at a particular index.
data Target = Prev | Next | Index Int
derive instance eqTarget :: Eq Target

-- | Represents whether the component should display the item container. You
-- | should use this in your render function to control visibility:
-- |
-- | ```purescript
-- | render state = if state.visibility == On then renderAll else renderInputOnly
-- | ```
data Visibility = Off | On
derive instance eqVisibility :: Eq Visibility
derive instance ordVisibility :: Ord Visibility

-- | Text-driven inputs will operate like a normal search-driven selection component.
-- | Toggle-driven inputs will capture key streams and debounce in reverse (only notify
-- | about searches when time has expired).
data InputType = Text | Toggle

-- | The component state
type State st =
  { inputType :: InputType
  , search :: String
  , debounceTime :: Milliseconds
  , debounceRef :: Maybe (Ref (Maybe Debouncer))
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int
  , getItemCount :: {| st } -> Int

  -- TODO
  -- , clickedInside :: Boolean
  -- , debug :: String
  | st
  }

type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  }

type Input st =
  { inputType :: InputType
  , search :: Maybe String
  , debounceTime :: Maybe Milliseconds
  , getItemCount :: {| st } -> Int

  -- TODO
  -- , debug :: String
  | st
  }

type Spec st query action slots input msg m =
  { -- usual Halogen component spec
    render
      :: State st
      -> H.ComponentHTML (Action action) slots m

    -- handle additional actionions provided to the component
  , handleAction
      :: action
      -> H.HalogenM (State st) (Action action) slots msg m Unit

    -- handle additional queries provided to the component
  , handleQuery
      :: forall a
      . query a
      -> H.HalogenM (State st) (Action action) slots msg m (Maybe a)

    -- handle messages emitted by the component; provide H.raise to simply
    -- raise the Select messages to the parent.
  , handleOutput
      :: Output
      -> H.HalogenM (State st) (Action action) slots msg m Unit

    -- optionally handle input on parent re-renders
  , receive
      :: input
      -> Maybe action

    -- perform some actionion when the component initializes.
  , initialize
      :: Maybe action

    -- optionally perform some actionion on initialization. disabled by default.
  , finalize
      :: Maybe action
  }

type Spec' st input m = Spec st (Const Void) Void () input Void m

defaultSpec
  :: forall st query action slots input msg m
  . Spec st query action slots input msg m
defaultSpec =
  { render: const (HH.text mempty)
  , handleAction: const (pure unit)
  , handleQuery: const (pure Nothing)
  , handleOutput: const (pure unit)
  , receive: const Nothing
  , initialize: Nothing
  , finalize: Nothing
  }

component
  :: forall st query action slots input msg m
  . MonadAff m
  => Row.Lacks "debounceRef" st
  => Row.Lacks "visibility" st
  => Row.Lacks "highlightedIndex" st
  -- => Row.Lacks "clickedInside" st
  => (input -> Input st)
  -> Spec st query action slots input msg m
  -> H.Component HH.HTML (Query query slots) input msg m
component mkInput spec = H.mkComponent
  { initialState: initialState <<< mkInput
  , render: spec.render
  , eval: H.mkEval
      { handleQuery: handleQuery spec.handleQuery
      , handleAction: handleAction spec.handleAction spec.handleOutput
      , initialize: Just (Initialize spec.initialize)
      , receive: map Action <<< spec.receive
      , finalize: map Action spec.finalize
      }
  }
  where
  initialState :: Input st -> State st
  initialState = Builder.build pipeline
    where
    pipeline =
      Builder.modify (SProxy :: _ "search") (fromMaybe "")
        >>> Builder.modify (SProxy :: _ "debounceTime") (fromMaybe mempty)
        >>> Builder.insert (SProxy :: _ "debounceRef") Nothing
        >>> Builder.insert (SProxy :: _ "visibility") Off
        >>> Builder.insert (SProxy :: _ "highlightedIndex") Nothing
        -- >>> Builder.insert (SProxy :: _ "clickedInside") false

handleQuery
  :: forall st query action slots msg m a
  . MonadAff m
  => (query a -> H.HalogenM (State st) (Action action) slots msg m (Maybe a))
  -> Query query slots a
  -> H.HalogenM (State st) (Action action) slots msg m (Maybe a)
handleQuery handleQuery' = case _ of
  Send box ->
    H.HalogenM $ liftF $ H.ChildQuery box

  Query query ->
    handleQuery' query

handleAction
  :: forall st action slots msg m
  . MonadAff m
  => Row.Lacks "debounceRef" st
  => Row.Lacks "visibility" st
  => Row.Lacks "highlightedIndex" st
  -- => Row.Lacks "clickedInside" st
  => (action -> H.HalogenM (State st) (Action action) slots msg m Unit)
  -> (Output -> H.HalogenM (State st) (Action action) slots msg m Unit)
  -> Action action
  -> H.HalogenM (State st) (Action action) slots msg m Unit
handleAction handleAction' handleOutput = case _ of
  Initialize mbAction -> do
    -- TODO debug
    -- debug <- H.gets _.debug
    -- H.liftEffect $ log $ debug <> ": Initialize"

    ref <- H.liftEffect $ Ref.new Nothing
    H.modify_ _ { debounceRef = Just ref }
    for_ mbAction handleAction'

  Search str -> do
    -- -- TODO debug
    -- debug <- H.gets _.debug
    -- H.liftEffect $ log $ debug <> ": Search"

    st <- H.get
    ref <- H.liftEffect $ map join $ traverse Ref.read st.debounceRef
    H.modify_ _ { search = str }
    void $ H.fork $ handle $ SetVisibility On

    case st.inputType /\ ref of
      Text /\ Nothing -> unit <$ do
        var <- H.liftAff AVar.empty
        fiber <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put unit var

        -- This compututation will fork and run in the background. When the
        -- var is finally filled, the actionion will run
        void $ H.fork do
          void $ H.liftAff $ AVar.take var
          void $ H.liftEffect $ traverse_ (Ref.write Nothing) st.debounceRef
          H.modify_ _ { highlightedIndex = Just 0 }
          newState <- H.get
          handleOutput $ Searched newState.search

        void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

      Text /\ Just debouncer -> do
        let var = debouncer.var
        void $ H.liftAff $ killFiber (error "Time's up!") debouncer.fiber
        fiber <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put unit var
        void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

      -- Key stream is not yet implemented. However, this should capture user
      -- key events and expire their search after a set number of milliseconds.
      _ -> pure unit

  Highlight target -> do
    -- -- TODO debug
    -- debug <- H.gets _.debug
    -- H.liftEffect $ log $ debug <> ": Highlight"

    st <- H.get
    when (st.visibility == On) do
      H.modify_ _ { highlightedIndex = Just $ getTargetIndex st target }

  Select target mbEv -> do
    -- -- TODO debug
    -- debug <- H.gets _.debug
    -- H.liftEffect $ log $ debug <> ": Select"

    for_ mbEv (H.liftEffect <<< preventDefault <<< ME.toEvent)
    st <- H.get
    when (st.visibility == On) case target of
      Index ix -> handleOutput $ Selected ix
      Next -> handleOutput $ Selected $ getTargetIndex st target
      Prev -> handleOutput $ Selected $ getTargetIndex st target

  ToggleClick ev -> do
    -- TODO debug
    -- debug <- H.gets _.debug
    -- H.liftEffect $ log $ debug <> ": ToggleClick"

    -- TODO
    H.liftEffect $ preventDefault $ ME.toEvent ev
    st <- H.get
    case st.visibility of
      On -> do
        -- TODO
        -- handle $ Focus false
        handle $ SetVisibility Off
      Off -> do
        -- TODO
        -- handle $ Focus true
        handle $ SetVisibility On

  Focus shouldFocus -> do
    -- TODO debug
    -- debug <- H.gets _.debug
    -- H.liftEffect $ log $ debug <> ": Focus"

    inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
    for_ inputElement \el -> H.liftEffect case shouldFocus of
      true -> HTMLElement.focus el
      _ -> HTMLElement.blur el

  Key ev -> do
    -- TODO debug
    -- debug <- H.gets _.debug
    -- H.liftEffect $ log $ debug <> ": Key"

    void $ H.fork $ handle $ SetVisibility On
    let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
    case KE.code ev of
      "ArrowUp" ->
        preventIt *> handle (Highlight Prev)
      "ArrowDown" ->
        preventIt *> handle (Highlight Next)
      "Escape" -> do
        inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
        preventIt
        for_ inputElement (H.liftEffect <<< HTMLElement.blur)
      "Enter" -> do
        st <- H.get
        preventIt
        for_ st.highlightedIndex \ix ->
          handle $ Select (Index ix) Nothing
      otherKey -> pure unit

  PreventClick ev ->
    H.liftEffect $ preventDefault $ ME.toEvent ev

  SetVisibility v -> do
    -- TODO debug
    -- debug <- H.gets _.debug
    -- H.liftEffect $ log $ debug <> ": SetVisibility " <> if v == On then "On" else "Off"

    st <- H.get
    when (st.visibility /= v) do
      H.modify_ _ { visibility = v, highlightedIndex = Just 0 }
      handleOutput $ VisibilityChanged v

  Action action -> handleAction' action

  -- OnMouseDownContainer me -> do
  --   -- TODO debug
  --   debug <- H.gets _.debug
  --   H.liftEffect $ log $ debug <> ": OnMouseDownContainer"

  --   -- TODO
  --   -- H.liftEffect $ preventDefault $ ME.toEvent me
  --   H.modify_ _ { clickedInside = true }
  -- OnMouseUpContainer me -> do
  --   -- TODO debug
  --   debug <- H.gets _.debug
  --   H.liftEffect $ log $ debug <> ": OnMouseUpContainer"

  --   -- TODO
  --   -- H.liftEffect $ preventDefault $ ME.toEvent me
  --   H.modify_ _ { clickedInside = false }
  -- OnBlur -> do
  --   -- TODO debug
  --   debug <- H.gets _.debug
  --   clickedInside <- H.gets _.clickedInside
  --   H.liftEffect $ log $ debug <> ": OnBlur, " <> "clickInside: " <> show clickedInside

  --   when (not clickedInside) do
  --     handle $ SetVisibility Off

  where
  -- eta-expansion is necessary to avoid infinite recursion
  handle action = handleAction handleAction' handleOutput action

  getTargetIndex st = case _ of
    Index i -> i
    Prev -> case st.highlightedIndex of
      Just i | i /= 0 -> i - 1
      _ -> lastIndex st
    Next -> case st.highlightedIndex of
      Just i | i /= lastIndex st -> i + 1
      _ -> 0
    where
    -- we know that the getItemCount function will only touch user fields,
    -- and that the state record contains *at least* the user fields, so
    -- this saves us from a set of unnecessary record deletions / modifications
    userState :: State st -> {| st }
    userState = unsafeCoerce

    lastIndex :: State st -> Int
    lastIndex = (_ - 1) <<< st.getItemCount <<< userState
