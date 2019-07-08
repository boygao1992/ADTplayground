module Ocelot.Component.SearchBar where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Fiber, delay, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Icon
import Ocelot.HTML.Properties (css)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent as ME

type Slot = H.Slot Query Output

type ComponentM  m a = H.HalogenM State Action ChildSlots Output m a
type Component m = H.Component HH.HTML Query Input Output m
type ComponentHTML m = H.ComponentHTML Action ChildSlots m
type ComponentRender m = State -> ComponentHTML m

type State =
  { query :: String
  , debouncer :: Maybe Debouncer
  , debounceTime :: Milliseconds
  , open :: Boolean
  }

type Debouncer =
  { var :: AVar String
  , fiber :: Fiber Unit
  }

data Action
  = Open
  | Blur
  | Clear ME.MouseEvent
  | Search String

data Query a
  = SetText String a

type Input =
  { debounceTime :: Maybe Milliseconds }

data Output
 = Searched String

type ChildSlots = ()

component :: forall m. MonadAff m => Component m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

initialState :: Input -> State
initialState { debounceTime } =
  { query: ""
  , debouncer: Nothing
  , debounceTime: fromMaybe (Milliseconds 0.0) debounceTime
  , open: false
  }

openIfHasQuery :: forall m. String -> ComponentM m Unit
openIfHasQuery q =
  if null q then pure unit else H.modify_ _ { open = true }

closeIfNullQuery :: forall m. String -> ComponentM m Unit
closeIfNullQuery q =
    if null q then H.modify_ _ { open = false } else pure unit

handleAction :: forall m. MonadAff m => Action -> ComponentM m Unit
handleAction = case _ of
  Open -> do
    H.modify_ _ { open = true }

  Blur -> do
    query <- H.gets _.query
    closeIfNullQuery query

  Clear ev -> do
    H.liftEffect $ stopPropagation $ ME.toEvent ev
    H.modify_ _ { query = "", open = false }
    H.raise $ Searched ""

  Search str -> do
    H.modify_ _ { query = str }
    openIfHasQuery str
    st <- H.get

    case st.debouncer of
      Nothing -> unit <$ do
        var <- H.liftAff AVar.empty
        fiber <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put str var

        _ <- H.fork do
          val <- H.liftAff $ AVar.take var
          H.modify_ _ { debouncer = Nothing }
          H.raise $ Searched val

        H.modify_ _ { debouncer = Just { var, fiber } }

      Just ({ var, fiber }) -> unit <$ do
        _ <- H.liftAff $ killFiber (error "Debounce restarted") fiber
        fiber' <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put str var

        H.modify_ _ { debouncer = Just { var, fiber: fiber' }}

handleQuery :: forall m a. Query a -> ComponentM m (Maybe a)
handleQuery = case _ of
  -- For when there is an existing search performed, but you need to set the
  -- field's text anyway.
  SetText str a -> Just a <$ do
    H.modify_ _ { query = str }
    openIfHasQuery str

render :: forall m. ComponentRender m
render { query, open } =
  HH.label
    [ HP.classes $ containerClasses <> containerCondClasses
    , HE.onClick (Just <<< const Open)
    ]
    [ HH.div
      [ HP.classes $ iconClasses <> iconCondClasses ]
      [ Icon.search_ ]
    , HH.div
      [ css "flex-grow" ]
      [ HH.input
        [ HE.onValueInput (Just <<< Search)
        , HP.placeholder "Search"
        , HP.value query
        , HP.classes $ inputClasses <> inputCondClasses
        , HE.onBlur (Just <<< const Blur)
        , HP.tabIndex 0
        ]
      ]
    , HH.button
      [ HE.onClick (Just <<< Clear)
      , HP.type_ HP.ButtonButton
      , HP.classes $ buttonClasses <> buttonCondClasses
      ]
      [ Icon.delete_ ]
    ]

  where
    containerClasses = HH.ClassName <$>
      [ "flex"
      , "no-outline"
      , "items-stretch"
      , "transition-1/4"
      , "border-b-2"
      , "group"
      ]

    containerCondClasses =
      ifOpen
        [ "max-w-160", "border-blue-88" ]
        [ "max-w-12", "border-transparent", "cursor-pointer" ]

    iconClasses = HH.ClassName <$>
      [ "pr-3"
      , "text-2xl"
      , "group-hover:text-grey-50"
      , "transition-1/4"
      ]

    iconCondClasses =
      ifOpen
        [ "text-grey-50", "mb-0", "mt-0" ]
        [ "text-grey-70", "-mb-1", "mt-1" ]

    inputClasses = HH.ClassName <$>
      [ "no-outline"
      , "flex-1"
      , "bg-transparent"
      , "h-full"
      , "transition-1/4"
      ]

    inputCondClasses =
      ifOpen
        [ "w-full" ]
        [ "w-0" ]

    buttonClasses = HH.ClassName <$>
      [ "no-outline"
      , "text-grey-70"
      , "hover:text-grey-50"
      , "text-xs"
      , "transition-1/4"
      , "flex-shrink"
      ]

    buttonCondClasses =
      ifOpen
        [ "opacity-100", "visible" ]
        [ "opacity-0", "invisible" ]

    ifOpen openClasses closedClasses =
      HH.ClassName <$> if open then openClasses else closedClasses
