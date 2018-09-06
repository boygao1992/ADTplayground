module Main where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (Free, foldFree, liftF)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_, traverse_)
import Effect.Aff (Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentDSL, ComponentHTML, component, liftAff, liftEffect, get, modify_, query) as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM (fork, raise) as H
import Renderless.State (getState, modifyState_, modifyStore)
import Web.Event.Event (preventDefault, currentTarget, Event) as E
import Web.HTML.HTMLElement (HTMLElement, blur, focus, fromEventTarget) as WHE
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-- | Component Types
type Component o item m
  = H.Component HH.HTML (Query o item) (Input o item) (Message o item) m

type ComponentHTML o item
  = H.ComponentHTML (Query o item)

type ComponentDSL o item m
  = H.ComponentDSL (StateStore o item) (Query o item) (Message o item) m

type StateStore o item
  = Store (State item) (ComponentHTML o item)

-- | Core Constructors
data QueryF o item next
  = Search String next
  -- | Highlight Target next
  -- | Select Int next
  -- | CaptureRef E.Event next
  -- | Focus Boolean next
  -- | Key KE.KeyboardEvent next
  -- | PreventClick ME.MouseEvent next
  | SetVisibility Visibility next
  -- | GetVisibility (Visibility -> next)
  -- | ReplaceItems (Array item) next
  -- | Raise (o Unit) next
  | Receive (Input o item) next

type Query o item = Free (QueryF o item) -- HACK: to chain multiple Query within eval' function which current eval function would not allow

setVisibility :: forall o item . Visibility -> Query o item Unit
setVisibility v = liftF (SetVisibility v unit)

-- | Highlight
-- | previous item / next item / item by index
data Target = Prev | Next | Index Int
derive instance eqTarget :: Eq Target

-- | Visibility
data Visibility = Off | On
derive instance eqVisibility :: Eq Visibility
derive instance ordVisibility :: Ord Visibility
instance heytingAlgebraVisibility :: HeytingAlgebra Visibility where
  tt = On
  ff = Off
  not On = Off
  not Off = On
  conj On On = On
  conj _ _ = Off
  disj Off Off = Off
  disj _ _ = On
  implies On Off = Off
  implies _ _ = On
-- | class (HeytingAlgebra a) <= BooleanAlgebra a
-- | follow Law of Excluded Middle: a || not a = tt
-- | (not p || q) = (p -> q)
instance booleanAlgebraVisibility :: BooleanAlgebra Visibility

data InputType
  = TextInput -- Text-driven input: normal search-driven selection component
  | Toggle -- Toggle-driven: debounce

type State item =
  { inputType :: InputType
  , search :: String
  , debounceTime :: Milliseconds
  , debouncer :: Maybe Debouncer
  , inputElement :: Maybe WHE.HTMLElement
  , items :: Array item
  , visibility :: Visibility
  , highlightedIndex :: Maybe Int
  , lastIndex :: Int
  }

type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  }

type Input o item =
  { inputType :: InputType -- differentiate Inputs by a coproduct
  , items :: Array item
  , initialSearch :: Maybe String
  , debounceTime :: Maybe Milliseconds
  , render :: State item -> ComponentHTML o item -- configurable render function
  }

data Message o item
  = Searched String
  | Selected item
  | VisibilityChanged Visibility
  | Emit (o Unit)

component
  :: forall o item m
   . MonadAff m =>
     Component o item m
component =
  H.component
    { initialState
    , render : extract
    , eval : eval'
    , reciever : Just $ HE.input Receive
    }
  where
    initialState i = store i.render
      { inputType: i.inputType
      , search: fromMaybe "" i.initialSearch
      , debounceTime: fromMaybe (Milliseconds 0.0) i.debounceTime
      , debouncer: Nothing
      , inputElement: Nothing
      , items: i.items
      , highlightedIndex: Nothing
      , visibility: Off
      , lastIndex: length i.items - 1
      }

    -- Construct the fold over the free monad based on the stepwise eval
    eval' :: Query o item ~> ComponentDSL o item m
    eval' a = foldFree eval a

    -- Helper for setting visibility inside `eval`. Eta-expanded bc strict
    -- mutual recursion woes.
    setVis v = eval' (setVisibility v)

    -- Just the normal Halogen eval
    eval :: QueryF o item ~> ComponentDSL o item m
    eval = case _ of
      Search str a -> a <$ do
        st <- getState
        modifyState_ _ { search = str }
        setVis On

        case st.inputType, st.debouncer of
          TextInput, Nothing -> unit <$ do
            var   <- H.liftAff AVar.empty
            fiber <- H.liftAff $ forkAff do
              delay st.debounceTime
              AVar.put unit var

            -- This compututation will fork and run in the background. When the
            -- var is finally filled, the .ct will run (raise a new search)
            _ <- H.fork do
              _ <- H.liftAff $ AVar.take var
              modifyState_ _ { debouncer = Nothing, highlightedIndex = Just 0 }
              newState <- getState
              H.raise $ Searched newState.search

            modifyState_ _ { debouncer = Just { var, fiber } }

          TextInput, Just debouncer -> do
            let var = debouncer.var
            _ <- H.liftAff $ killFiber (error "Time's up!") debouncer.fiber
            fiber <- H.liftAff $ forkAff do
              delay st.debounceTime
              AVar.put unit var

            modifyState_ _ { debouncer = Just { var, fiber } }

          -- Key stream is not yet implemented. However, this should capture user
          -- key events and expire their search after a set number of milliseconds.
          _, _ -> pure unit

      -- Highlight target a -> a <$ do
      --   st <- getState
      --   when (st.visibility /= Off) $ do
      --     let highlightedIndex = case target of
      --           Prev  -> case st.highlightedIndex of
      --             Just i | i /= 0 ->
      --               Just (i - 1)
      --             _ ->
      --               Just st.lastIndex
      --           Next  -> case st.highlightedIndex of
      --             Just i | i /= st.lastIndex ->
      --               Just (i + 1)
      --             _ ->
      --               Just 0
      --           Index i ->
      --             Just i
      --     modifyState_ _ { highlightedIndex = highlightedIndex }
      --   pure unit

      -- Select index a -> a <$ do
      --   st <- getState
      --   when (st.visibility == On) $
      --     for_ (st.items !! index)
      --       \item -> H.raise (Selected item)

      -- CaptureRef event a -> a <$ do
      --   st <- getState
      --   modifyState_ _ { inputElement = fromEventTarget =<< currentTarget event }
      --   pure a

      -- Focus focusOrBlur a -> a <$ do
      --   st <- getState
      --   traverse_ (H.liftEffect <<< if focusOrBlur then focus else blur) st.inputElement

      -- Key ev a -> a <$ do
      --   setVis On
      --   let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
      --   case KE.code ev of
      --     "ArrowUp"   -> preventIt *> eval' (highlight Prev)
      --     "ArrowDown" -> preventIt *> eval' (highlight Next)
      --     "Escape"    -> do
      --       st <- getState
      --       preventIt
      --       for_ st.inputElement (H.liftEffect <<< blur)
      --     "Enter"     -> do
      --       st <- getState
      --       preventIt
      --       for_ st.highlightedIndex (eval' <<< select)
      --     otherKey    -> pure unit

      -- PreventClick ev a -> a <$ do
      --   H.liftEffect <<< preventDefault <<< ME.toEvent $ ev

      SetVisibility v a -> a <$ do
        st <- getState
        when (st.visibility /= v) do
          modifyState_ _ { visibility = v, highlightedIndex = Just 0 }
          H.raise $ VisibilityChanged v

      -- GetVisibility f -> do
      --   st <- getState
      --   pure (f st.visibility)

      -- ReplaceItems items a -> a <$ do
      --   modifyState_ _
      --     { items = items
      --     , lastIndex = length items - 1
      --     , highlightedIndex = Nothing }

      -- Raise parentQuery a -> a <$ do
      --   H.raise (Emit parentQuery)

      Receive input a -> a <$ do
        modifyStore input.render identity
