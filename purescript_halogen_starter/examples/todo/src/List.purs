module List where

import Prelude

import Data.Array (length, snoc, filter)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Console (log)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Task as Task

type TaskId = Int

type State =
  { tasks :: Array TaskId
  , nextId :: TaskId
  , numCompleted :: Int
  }

data Query next
  = NewTask next
  | AllDone next
  | HandleTaskMessage TaskId Task.Output next

type Input = Unit

type Output = Void

type ChildQuery = Task.Query

newtype ChildSlot = ChildSlot TaskId
derive newtype instance eqChildSlot :: Eq ChildSlot
derive newtype instance ordChildSlot :: Ord ChildSlot
derive newtype instance showChildSlot :: Show ChildSlot

component :: H.Component HH.HTML Query Input Output Aff
-- component :: forall m. H.Component HH.HTML Query Input Output m
component =
  H.parentComponent
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
  where
    initialState :: State
    initialState =
      { tasks : []
      , nextId : 0
      , numCompleted : 0
      }

    render :: forall m. State -> H.ParentHTML Query ChildQuery ChildSlot m
    render { tasks, numCompleted } =
      HH.div_
        [ HH.h1_
            [ HH.text "Todo list" ]
        , HH.p_
            [ HH.button [ HE.onClick (HE.input_ NewTask) ]
                [ HH.text "New Task" ]
            ]
        , HH.ul_ $ map renderTask tasks
        , HH.p_
            [ HH.text $ show numCompleted <> " / " <> show (length tasks) <> " complete" ]
        , HH.button [ HE.onClick (HE.input_ AllDone) ]
            [ HH.text "All done" ]
        ]

    renderTask :: forall m. TaskId -> H.ParentHTML Query ChildQuery ChildSlot m
    renderTask id =
      HH.slot
        (ChildSlot id)
        Task.component
        unit
        (HE.input (HandleTaskMessage id))

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output Aff
    eval (NewTask next) = next <$ do
      H.modify_ $ \state -> state { nextId = state.nextId + 1, tasks = state.tasks `snoc` state.nextId }
    eval (AllDone next) = next <$ do
      toggled <- H.queryAll (H.request (Task.ToggleCompletedFromParent true))
      H.liftEffect $ log $ show $ toggled
      H.modify_ $ \state -> state { numCompleted = Map.size <<< Map.filter identity$ toggled }
    eval (HandleTaskMessage id output next) = next <$ do
      case output of
        Task.Removed -> do
          wasCompleted <- H.query (ChildSlot id) (H.request Task.IsCompleted)
          when (fromMaybe false wasCompleted)
            $ H.modify_ $ \state -> state { numCompleted = state.numCompleted - 1 }
          H.modify_ $ \state -> state { tasks = filter (_ /= id) state.tasks }
        Task.Toggled b -> do
          H.modify_ $ \state -> state { numCompleted = if b then state.numCompleted + 1 else state.numCompleted - 1 }
          H.liftEffect $ log $ "Toggled: " <> show id
