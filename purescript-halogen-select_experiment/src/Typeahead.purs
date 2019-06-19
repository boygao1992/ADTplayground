module Typeahead where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AR
import Data.Argonaut.Decode ((.:), decodeJson)
import Data.Array ((:), (!!))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_, traverse)
import Data.Tuple.Nested ((/\))
import Dropdown as Dropdown
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData as RD
import Select as S
import Select.Setters as SS
import Type.Data.Symbol (SProxy(..))

searchLocations :: String -> Aff (RD.RemoteData String (Array Location))
searchLocations search = do
  res <- AX.get AR.json ("https://swapi.co/api/planets/?search=" <> search)
  let body = lmap AR.printResponseFormatError res.body
  pure
    $ RD.fromEither
    $ traverse decodeJson
    =<< (_ .: "results")
    =<< decodeJson
    =<< body
type Location =
  { name :: String
  , population :: String
  }

type StateRow =
  ( selections :: Array Location
  , available :: RD.RemoteData String (Array Location)
  )

type EmbeddedState = S.State StateRow

data Action
  = Remove Location
  | HandleDropdown Dropdown.Output

type EmbeddedAction = S.Action Action

data Query a
  = GetSelections (Array Location -> a)

type EmbeddedQuery = S.Query Query ChildSlots

type Input = Unit

type EmbeddedInput = S.Input StateRow

data Output
  = ItemRemoved Location
  | SelectionsChanged (Array Location)

type ChildSlots =
  ( dropdown :: Dropdown.EmbeddedSlot Unit
  )

_dropdown = SProxy :: SProxy "dropdown"

type EmbeddedComponentM m a
  = H.HalogenM EmbeddedState EmbeddedAction ChildSlots Output m a
type EmbeddedComponent m = H.Component HH.HTML EmbeddedQuery Input Output m
type EmbeddedComponentHTML m = H.ComponentHTML EmbeddedAction ChildSlots m
type EmbeddedComponentRender m = EmbeddedState -> EmbeddedComponentHTML m

type EmbeddedSlot = S.Slot Query ChildSlots Output

inputAdapter :: Input -> EmbeddedInput
inputAdapter _ =
  { inputType: S.Text
  , debounceTime: Just (Milliseconds 300.0)
  , search: Nothing
  , getItemCount: maybe 0 Array.length <<< RD.toMaybe <<< _.available
  , selections: []
  , available: RD.NotAsked

  , debug: "Typeahead"
  }

component :: forall m. MonadAff m => EmbeddedComponent m
component = S.component inputAdapter $ S.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleQuery = handleQuery
  , handleOutput = handleOutput
  }

render :: forall m. MonadAff m => EmbeddedComponentRender m
render st =
  HH.div_
  [ renderSelections
  , renderInput
  , renderContainer
  ]

  where
    renderSelections :: EmbeddedComponentHTML m
    renderSelections =
      HH.div_
      (renderSelectionItem <$> st.selections)
      where
        renderSelectionItem :: Location -> EmbeddedComponentHTML m
        renderSelectionItem item =
          HH.div_
          [ HH.span_
            [ HH.text item.name ]
          , renderCloseButton item
          ]

        renderCloseButton :: Location -> EmbeddedComponentHTML m
        renderCloseButton item =
          HH.span [ HE.onClick $ Just <<< S.Action <<< const (Remove item) ]
          [ HH.text "x" ]

    renderInput :: EmbeddedComponentHTML m
    renderInput =
      HH.input $ SS.setInputProps
                 [ HP.placeholder "Type to search ..." ]

    renderDropdown :: EmbeddedComponentHTML m
    renderDropdown =
      if st.visibility == S.On
      then
        HH.slot _dropdown unit Dropdown.component
          dropdownInput
          (Just <<< S.Action <<< HandleDropdown)
      else
        HH.text ""
      where
        dropdownInput =
          { items: [ "Earth", "Mars" ]
          , buttonLabel: "Human Planets"
          }

    renderContainer :: EmbeddedComponentHTML m
    renderContainer =
      if st.visibility == S.On
      then
        HH.div (SS.setContainerProps [])
        ([renderDropdown]
         <> renderItems
        )
      else
        HH.text ""
      where
        hasItems = maybe false (not <<< Array.null) (RD.toMaybe st.available)

        renderItems = do
          let renderMsg msg = [ HH.span_ [ HH.text msg ] ]
          case st.available of
            RD.NotAsked -> renderMsg "No search performed..."
            RD.Loading -> renderMsg "Loading..."
            RD.Failure e -> renderMsg e
            RD.Success available
              | hasItems -> Array.mapWithIndex renderItem available
              | otherwise -> renderMsg "No results found"

        renderItem :: Int -> Location -> EmbeddedComponentHTML m
        renderItem idx { name, population } =
          HH.div ( SS.setItemProps idx [] )
          [ HH.span []
            [ HH.text name ]
          , HH.span []
            [ HH.text population ]
          ]

handleAction :: forall m. MonadAff m => Action -> EmbeddedComponentM m Unit
handleAction = case _ of
  Remove item -> do
    selections <- H.gets $ Array.filter (_ /= item) <<< _.selections
    H.modify_ _ { selections = selections }
    H.raise $ ItemRemoved item

  HandleDropdown (Dropdown.SelectionChanged { prev, current }) -> do
    st <- H.get
    let
      mkLocation str = { name: "User Added: " <> str, population: "1" }
      newSelections = case prev /\ current of
        Nothing /\ Nothing ->
          Nothing
        Nothing /\ Just str ->
          Just (mkLocation str : st.selections)
        Just str /\ Nothing ->
          Just (Array.filter (_ /= mkLocation str) st.selections)
        Just old /\ Just new ->
          Just (mkLocation new : (Array.filter (_ /= mkLocation old) st.selections))
    for_ newSelections \selections ->
      H.modify_ _ { selections = selections }

handleQuery :: forall m a. MonadAff m => Query a -> EmbeddedComponentM m (Maybe a)
handleQuery = case _ of
  GetSelections reply -> do
    selections <- H.gets _.selections
    pure $ Just $ reply selections

handleOutput :: forall m . MonadAff m => S.Output -> EmbeddedComponentM m Unit
handleOutput = case _ of
  S.Selected ix -> do
    st <- H.get
    for_ st.available \arr ->
      for_ (arr !! ix) \item -> do
        let newSelections = item : st.selections
        H.modify_ _ { selections = item : st.selections
                    , available = RD.Success (Array.filter (_ /= item) arr)
                    , search = ""
                    }
        H.raise $ SelectionsChanged newSelections
  S.Searched str -> do
    st <- H.get
    -- we'll use an external api to search locations
    H.modify_ _ { available = RD.Loading }
    items <- H.liftAff $ searchLocations str
    H.modify_ _ { available = items <#> \xs -> Array.difference xs st.selections }
  _ -> pure unit
