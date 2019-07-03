module Adhoc.Cell.Renderless where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array ((!!))
import Data.Array as Array
import Data.Fixed as Fixed
import Data.Formatter.Number (Formatter(..), format) as Number
import Data.Int (fromString, toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.String as String
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Renderless.State (Store, extract, getState, getsState, modifyState_, modifyStore_, store)
import Web.DOM.Element as WDE
import Web.Event.Event as WE
import Web.HTML.HTMLElement as WHE
import Web.UIEvent.KeyboardEvent as KE

data CellType
  = CellTypeInt Int
  | CellTypeNumber Number
  | CellTypeFixed Int Number
  | CellTypeString String
  | CellTypeEnum (Array String) String
derive instance eqCellType :: Eq CellType
derive instance ordCellType :: Ord CellType
instance showCellType :: Show CellType where
  show (CellTypeInt int) = show int
  show (CellTypeNumber num) = show num
  show (CellTypeFixed p num) = show num
  show (CellTypeString str) = str
  show (CellTypeEnum _ str) = str

displayCellType :: CellType -> String
displayCellType = case _ of
  CellTypeInt int -> Number.format (numberFormatter 0) <<< Int.toNumber $ int
  CellTypeNumber num -> Number.format (numberFormatter 3) num
  CellTypeFixed p num -> Number.format (numberFormatter p) num
  CellTypeString str -> str
  CellTypeEnum _ str -> str

  where
    numberFormatter :: Int -> Number.Formatter
    numberFormatter precision = Number.Formatter
      { comma: true
      , before: 0
      , after: precision
      , abbreviations: false
      , sign: false
      }

fromString :: CellType -> String -> Maybe CellType
fromString cellType input = case cellType of
  CellTypeInt _ -> CellTypeInt <$> Int.fromString input
  CellTypeNumber _ -> CellTypeNumber <$> Number.fromString input
  CellTypeFixed p _ -> CellTypeFixed p <$> join (Fixed.reifyPrecision p (map Fixed.toNumber <<< fromString' input))
  CellTypeString _ -> Just $ CellTypeString input
  CellTypeEnum available _ ->
    if (Array.elem input available)
    then Just $ CellTypeEnum available input
    else Nothing
  where
    fromString' :: forall precision. Fixed.KnownPrecision precision => String -> Fixed.PProxy precision -> Maybe (Fixed.Fixed precision)
    fromString' x _ = Fixed.fromString x

type State =
  { editing :: Boolean
  , cache :: String
  , width :: Number

  , isOpen :: Boolean
  , highlightedIndex :: Int

  , candidates :: Array String

  , displayMessage :: Maybe String
  , value :: CellType
  }

type StateStore m = Store State (ComponentHTML m)

defaultInitialState :: State
defaultInitialState =
  { editing: false
  , cache: ""
  , width: 0.0

  , isOpen: false
  , highlightedIndex: 0

  , candidates: []

  , displayMessage: Nothing
  , value: CellTypeString ""
  }

data Action m
  = Edit
  | OnValueInput String
  | OnKeyDown KE.KeyboardEvent
  | OnBlurInput -- TODO replace by ??
  -- | OnMouseDownToggle
  | OnClickItem Int
  | OnMouseEnterItem Int
  | Receive (Input m)

data Query a
  = GetValue (CellType -> a)

type Input m =
  { render :: ComponentRender m
  -- External State
  -- , available :: Array String
  -- Config
  , value :: CellType
  }

type Output = Void

type ChildSlots = ()

type ComponentM m a = H.HalogenM (StateStore m) (Action m) ChildSlots Output m a
type Component m = H.Component HH.HTML Query (Input m) Output m
type ComponentHTML m = H.ComponentHTML (Action m) ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type Slot = H.Slot Query Output

inputRef = H.RefLabel "__cell_input" :: H.RefLabel
ghostRef = H.RefLabel "__cell_ghostEl" :: H.RefLabel
menuRef = H.RefLabel "__nselect_menu" :: H.RefLabel
selectedItemRef = H.RefLabel "__nselect_selectedItem" :: H.RefLabel

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState: \({ render, value }) ->
     store render defaultInitialState
      { value = value
      -- , available = available
      }
  , render: extract
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
  }

handleAction :: forall m. MonadAff m => Action m -> ComponentM m Unit
handleAction = case _ of
  Edit -> do
    modifyState_ \st -> st { editing = true, cache = show st.value }
    updateInputWidth
    focusInput

  OnValueInput str -> do
    modifyState_ \st -> st
          { cache = str
          , displayMessage = Nothing
          }
    updateInputWidth

    getsState _.value >>= case _ of
      CellTypeEnum available _ -> do
        let candidates = Array.filter (String.contains (String.Pattern str)) available
        modifyState_ \st -> st
          { candidates = candidates
          -- , highlightedIndex = 0
          }
        handleMenuVisibilityChange $ Array.length candidates > 0
      _ -> pure unit

  OnKeyDown ke -> do
    case KE.key ke of
      "ArrowUp" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        s <- getState
        when s.isOpen do
          let nextIndex = max 0 (s.highlightedIndex - 1)
          when (nextIndex /= s.highlightedIndex) do
            handleHighlightedIndexChange nextIndex
      "ArrowDown" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        s <- getState
        when s.isOpen do
          let nextIndex
                = min (Array.length s.candidates - 1) (s.highlightedIndex + 1)
          when (nextIndex /= s.highlightedIndex) do
            handleHighlightedIndexChange nextIndex
      "Enter" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        s <- getState
        if s.isOpen
          then do
            when (Array.length s.candidates > 0)
              $ for_ (s.candidates !! s.highlightedIndex) \selection -> do
                modifyState_ _ { cache = selection }
                handleMenuVisibilityChange false
                updateInputWidth
          else do
            case fromString s.value s.cache of
              Nothing ->
                modifyState_
                  _ { displayMessage = Just $ "not an valid " <> case s.value of
                        CellTypeInt _ -> "integer"
                        CellTypeNumber _ -> "number"
                        CellTypeFixed _ _ -> "number"
                        CellTypeString _ -> "input"
                        CellTypeEnum _ _ -> "option"
                    }
              Just value ->
                modifyState_ _ { editing = false, value = value }

            -- case s.value of
            --   CellTypeEnum _ _ ->
            --     when (not Array.elem s.cache s.available) do
            --       modifyState_ _ { displayMessage = Just "not an valid option" }
            --   _ ->
            --     modifyState_ _ { editing = false, value = s.cache }

      "Escape" -> do
        H.liftEffect $ WE.preventDefault $ KE.toEvent ke
        -- isOpen <- getsState _.isOpen
        -- if isOpen
        --   then do
        --     handleMenuVisibilityChange false
        --   else do
        --     modifyState_ _ { editing = false }
        handleMenuVisibilityChange false
        modifyState_ _ { editing = false }

      _ -> do
        pure unit

  OnBlurInput -> do
    isOpen <- getsState _.isOpen
    if isOpen
      then do
        handleMenuVisibilityChange false
        focusInput
      else do
        handleMenuVisibilityChange false
        modifyState_ _ { editing = false }

  -- OnMouseDownToggle -> do
  --   isOpen <- getsState _.isOpen
  --   handleMenuVisibilityChange $ not isOpen

  OnClickItem index -> do
    getsState _.value >>= case _ of
      CellTypeEnum available _ -> do
        for_ (available !! index) \selection -> do
          modifyState_ _ { cache = selection }
          handleMenuVisibilityChange false
          updateInputWidth
      _ -> pure unit

  OnMouseEnterItem index -> do
    getsState _.value >>= case _ of
      CellTypeEnum _ _ -> do
        modifyState_ _ { highlightedIndex = index }
      _ -> pure unit

  Receive { render } -> do
    modifyStore_ render identity

  where
    updateInputWidth :: ComponentM m Unit
    updateInputWidth =
      void $ runMaybeT do
        ghostEl <- MaybeT $ H.getHTMLElementRef ghostRef
        textWidth
          <- H.lift <<< H.liftEffect
          $ map _.width
          $ WHE.getBoundingClientRect
          $ ghostEl
        H.lift $ modifyState_ _ { width = textWidth }

    focusInput :: ComponentM m Unit
    focusInput =
      void $ runMaybeT do
        el <- MaybeT $ H.getHTMLElementRef inputRef
        H.lift $ H.liftEffect $ WHE.focus el

    handleMenuVisibilityChange isOpen = do
      modifyState_ _ { isOpen = isOpen }
      -- Make sure highlighted item is visible when dropdown becomes open.
      when isOpen $
        scrollIntoViewIfNeeded

    handleHighlightedIndexChange index = do
      modifyState_ _ { highlightedIndex = index }
      scrollIntoViewIfNeeded

    scrollIntoViewIfNeeded = do
      void $ runMaybeT do
        menu <- MaybeT $ H.getHTMLElementRef menuRef
        let menuEl = WHE.toElement menu
        item <- MaybeT $ H.getHTMLElementRef selectedItemRef
        let itemEl = WHE.toElement item

        H.lift $ H.liftEffect do
          itemOffsetTop <- WHE.offsetTop item
          itemHeight <- WDE.clientHeight itemEl
          scrollTop <- WDE.scrollTop menuEl
          menuHeight <- WDE.clientHeight menuEl
          let itemBelow = itemOffsetTop + itemHeight > scrollTop + menuHeight
              itemAbove = itemOffsetTop < scrollTop
          if itemBelow
            then WDE.setScrollTop (itemOffsetTop + itemHeight - menuHeight) menuEl
            else if itemAbove
                then WDE.setScrollTop itemOffsetTop menuEl
                else pure unit

handleQuery :: forall m a. MonadAff m => Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  GetValue reply -> do
    value <- getsState _.value
    pure $ Just <<< reply $ value
