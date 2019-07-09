module Ocelot.Components.Tree.Component where


import Prelude

import Data.Array as A
import Data.Lens (class Wander, Lens', Optic', over, set)
import Data.Lens.Index (ix) as Lens
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (checked) as HP
import Ocelot.Block.Checkbox as Checkbox
import Ocelot.Block.Icon as Icon
import Ocelot.Data.Tree (ItemPath, Node(..), IndexPath, _expanded, _selected, _children)
import Ocelot.HTML.Properties (css)
import Renderless.State (Store, extract, getState, modifyState_, store)

type RenderItem item = item -> HH.PlainHTML

type Slot item = H.Slot (Query item) (Output item)

type ComponentM item m a = H.HalogenM (StateStore item m) (Action item) ChildSlots (Output item) m a
type Component item m = H.Component HH.HTML (Query item) (Input item) (Output item) m
type ComponentHTML item m = H.ComponentHTML (Action item) ChildSlots m
type ComponentRender item m = State item -> ComponentHTML item m

type State item =
  { items :: Array (Node item)
  , initial :: Array (Node item)
  }

type StateStore item m = Store (State item) (ComponentHTML item m)

data Action item
  = ToggleItem item (ItemPath item) IndexPath Boolean
  | ToggleChildren IndexPath

data Query item a
  = SetItems (Array (Node item)) a
  | SetSelections (Array (ItemPath item)) a

type Input item =
  { renderItem :: RenderItem item }

data Output item
  = ItemAdded item (ItemPath item)
  | ItemRemoved item (ItemPath item)

type ChildSlots = ()

component
  :: forall item m
  . Eq item
  => MonadAff m
  => Component item m
component = H.mkComponent
  { initialState
  , render: extract
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

initialState :: forall item m. Input item -> StateStore item m
initialState { renderItem } =
  store (renderAdapter renderItem)
    { items: []
    , initial: []
    }

renderAdapter
  :: forall item m
  . RenderItem item
  -> ComponentRender item m
renderAdapter renderItem { items } =
  HH.div_ $ A.concat $ A.mapWithIndex (renderRow 0 [] []) items
  where
  renderRow depth indexPath itemPath ix (Node { selected, expanded, children, value }) =
    [ HH.div
      [ css $ "border-b py-2 " <> ("pl-" <> (show (depth * 10))) ]
      [ renderCarat children expanded (A.cons ix indexPath)
      , HH.div
        [ css "inline-flex" ]
        [ Checkbox.checkbox_
          [ HE.onChecked $ Just <<< ToggleItem value itemPath (A.cons ix indexPath)
          , HP.checked selected
          ]
          [ HH.fromPlainHTML $ renderItem value ]
        ]
      ]
    ] <>
    ( if not expanded then [] else
      [ HH.div_
        ( A.concat
          $ A.mapWithIndex
            (renderRow (depth + 1) (A.cons ix indexPath) (A.snoc itemPath value))
            children
        )
      ]
    )

  renderCarat children expanded path =
    carat
      [ HE.onClick $ const <<< Just $ ToggleChildren path
      , css $ "mr-3 text-xl align-text-bottom cursor-pointer " <> visible
      ]
    where
      carat = if expanded then Icon.caratDown else Icon.caratRight
      visible = if A.length children > 0 then "visible" else "invisible"

handleAction :: forall item m. MonadAff m => Action item -> ComponentM item m Unit
handleAction = case _ of
  ToggleItem item itemPath indexPath checked-> do
    let pathLens = pathToLens indexPath _selected
    for_ pathLens \l -> do
      modifyState_ $ set l checked
    if checked
      then H.raise (ItemAdded item itemPath)
      else H.raise (ItemRemoved item itemPath)

  ToggleChildren indexPath -> do
    let pathLens = pathToLens indexPath _expanded
    for_ pathLens \l -> do
      modifyState_ $ over l not

handleQuery
  :: forall item m a
  . Eq item
  => MonadAff m
  => Query item a
  -> ComponentM item m (Maybe a)
handleQuery = case _ of
  SetItems items a -> Just a <$ do
    modifyState_ _ { items = items, initial = items }

  SetSelections itemPaths a -> Just a <$ do
    ({ items, initial }) <- getState
    let paths = flip itemPathToIndexPath items <$> itemPaths
        updates = paths <#> \p r -> r { items = expandPath p r.items }
        updater = A.foldl (>>>) (_ { items = initial }) updates
    modifyState_ updater

-----
-- Helper functions for expanding paths, toggling checkboxes, etc.

_items :: ∀ item. Lens' (State item) (Array (Node item))
_items = prop (SProxy :: SProxy "items")

pathToLens
  :: ∀ p a
  . Wander p
  => IndexPath
  -> Optic' p (Node a) Boolean
  -> Maybe (Optic' p (State a) Boolean)
pathToLens path lastProp = (<<<) _items <$> pathToLens'
  where
    pathToLens' :: Maybe (Optic' p (Array (Node a)) Boolean)
    pathToLens' = A.foldl foldLens <$> (last <$> A.head path) <*> A.tail path

    foldLens :: Optic' p (Array (Node a)) Boolean -> Int -> Optic' p (Array (Node a)) Boolean
    foldLens l ix = Lens.ix ix <<< _children <<< l

    last :: Int -> Optic' p (Array (Node a)) Boolean
    last ix = Lens.ix ix <<< lastProp

-- TODO : update this to use lenses, possibly using pathToLens on increasing subsections of array
--   e.g. [pathToLens [0] _expanded, pathToLens [0, 2] _expanded, pathToLens [0, 2, 1] _checked]
expandPath :: ∀ a. IndexPath -> Array (Node a) -> Array (Node a)
expandPath path traits = do
  expandPath' (A.head path) (A.tail path) traits
  where
    expandPath' (Just ix) (Just p) ts | A.length p > 0 = fromMaybe [] $ A.modifyAt ix (expand p) ts
                                      | otherwise = fromMaybe [] $ A.modifyAt ix check ts
    expandPath' _ _ ts = ts
    expand p (Node t) = Node $ t
      { expanded = true
      , children = expandPath' (A.head p) (A.tail p) t.children
      }
    check (Node t) = Node $ t { selected = true }

itemPathToIndexPath :: ∀ a. Eq a => ItemPath a -> Array (Node a) -> IndexPath
itemPathToIndexPath path ns =
  fst $ A.foldl makePath (Tuple [] ns) path
  where
    makePath (Tuple path' ns') item =
      Tuple
        (fromMaybe [] $ A.snoc path' <$> ix)
        (fromMaybe [] $ (_.children <<< unwrap) <$> (A.index ns' =<< ix))
      where
        ix = A.findIndex (\(Node node) -> node.value == item) ns'
