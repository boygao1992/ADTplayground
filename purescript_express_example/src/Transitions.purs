module Transitions where

import Prelude

import Data.Array (deleteAt, length, mapWithIndex, modifyAt, snoc) as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Types (Todo, TodoId, Result, AppState, IndexedTodo)

-- State Transition Functions with Error Handling
-- Ref.modify' :: forall s b. (s -> { state :: s, value :: b }) -> Ref s -> Effect b

addTodo :: Todo -> AppState -> Result TodoId
addTodo todo state =
  { state : state `A.snoc` todo
  , value : Right $ A.length state + 1
  }

updateTodo :: TodoId -> String -> AppState -> Result Unit
updateTodo id newDesc state =
  case A.modifyAt id (_ { description = newDesc }) state of
    Nothing ->
      { state
      , value : Left "No such Id."
      }
    Just newState ->
      { state : newState
      , value : Right unit
      }

deleteTodo :: TodoId -> AppState -> Result Unit
deleteTodo id state =
  case A.deleteAt id state of
    Nothing ->
      { state
      , value : Left "No such Id."
      }
    Just newState ->
      { state : newState
      , value : Right unit
      }

doneTodo :: Int -> AppState -> Result Unit
doneTodo id state =
  case A.modifyAt id (_ { isDone = true }) state of
    Nothing  ->
      { state
      , value : Left "No such Id."
      }
    Just newState ->
      { state: newState
      , value: Right unit
      }

getTodoWithIndexes :: AppState -> Array IndexedTodo
getTodoWithIndexes state =
  A.mapWithIndex addId state
  where
    addId :: TodoId -> Todo -> IndexedTodo
    addId id { description, isDone } = { id, description, isDone }
