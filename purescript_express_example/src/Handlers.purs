module Handlers where

import Prelude

import Data.Array (length) as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString) as Int
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Effect.Exception (Error)
import Effect.Exception (message, error) as Exception
import Effect.Ref (modify', read) as Ref
import Node.Express.Handler (Handler)
import Node.Express.Handler (next) as E
import Node.Express.Request (getOriginalUrl, getQueryParam, getRouteParam, getUserData, setUserData) as E
import Node.Express.Response (sendJson, setStatus) as E

import Types (AppStateRef)
import Transitions (addTodo, deleteTodo, doneTodo, getTodoWithIndexes)
import RouteParams (id) as RP
import QueryParams (description) as QP

respondError :: Error -> Handler
respondError err = do
  E.setStatus 400
  E.sendJson { error : Exception.message err }

-- logger
logger :: AppStateRef -> Handler
logger stateRef = do
  state <- liftEffect $ Ref.read stateRef
  url <- E.getOriginalUrl
  liftEffect $ Console.log $ ">>> " <> url <> " count =" <> (show $ A.length state)
  E.setUserData "logged" url
  E.next

-- /
indexHandler :: AppStateRef -> Handler
indexHandler _ = do
  E.sendJson help
  where
    help =
      { name : "PureScript-Express Example"
      , endpoints :
        { listTodos : "/todos"
        , createTodo : "/todos/create?desc=Do+something"
        , updateTodo : "/todos/update/:id?desc=Do+something+else"
        , doneTodo : "/todos/done/:id"
        , deleteTodo : "/todos/delete/:id"
        }
      }

-- /list
listTodosHandler :: AppStateRef -> Handler
listTodosHandler stateRef = do
  state <- liftEffect $ Ref.read stateRef
  E.sendJson $ getTodoWithIndexes state

-- /create?desc=Do+something
createTodoHandler :: AppStateRef -> Handler
createTodoHandler stateRef = do
  descParam <- E.getQueryParam QP.description
  case descParam of
    Nothing ->
      respondError (Exception.error "Description is required.")
    Just desc -> do
      newId <- liftEffect
        $ Ref.modify' (addTodo { description: desc, isDone: false}) stateRef
      E.sendJson { status: "Created", id: newId}

-- /update/:id?desc=Do+something+else
updateTodoHandler :: AppStateRef -> Handler
updateTodoHandler stateRef = do
  idParam <- E.getRouteParam RP.id
  descParam <- E.getQueryParam QP.description
  case idParam, descParam of
    Nothing, Nothing ->
      respondError (Exception.error "Both Id and Description are required.")
    Nothing, Just _ -> respondError (Exception.error "Id is required.")
    Just _, Nothing -> respondError (Exception.error "Description is required.")
    Just idLiteral, Just desc ->
      case Int.fromString idLiteral of
        Nothing -> respondError (Exception.error "Id must be an integer.")
        Just id -> do
          res <- liftEffect $ Ref.modify' (deleteTodo id) stateRef
          case res of
            Left msg -> respondError (Exception.error msg)
            Right _ -> E.sendJson { status : "Updated" }

-- /done/:id
doneTodoHandler :: AppStateRef -> Handler
doneTodoHandler stateRef = do
  idParam <- E.getRouteParam RP.id
  case idParam of
    Nothing -> respondError (Exception.error "Id is required.")
    Just idLiteral ->
      case Int.fromString idLiteral of
        Nothing -> respondError (Exception.error "Id must be an integer.")
        Just id -> do
          res <- liftEffect $ Ref.modify' (doneTodo id) stateRef
          case res of
            Left msg -> respondError (Exception.error msg)
            _ -> E.sendJson {status: "Done"}

-- /delete/:id
deleteTodoHandler :: AppStateRef -> Handler
deleteTodoHandler stateRef = do
  idParam <- E.getRouteParam RP.id
  case idParam of
    Nothing -> respondError (Exception.error "Id is required.")
    Just idLiteral ->
      case Int.fromString idLiteral of
        Nothing -> respondError (Exception.error "Id must be an integer.")
        Just id -> do
          res <- liftEffect $ Ref.modify' (deleteTodo id) stateRef
          case res of
            Left msg -> respondError (Exception.error msg)
            _ -> E.sendJson {status: "Deleted"}

-- /logger
getLoggerStatusHandler :: AppStateRef -> Handler
getLoggerStatusHandler _ = do
  userdata <- E.getUserData "logged"
  E.sendJson $ fromMaybe "missing" userdata

-- global error handler
errorHandler :: AppStateRef -> Error -> Handler
errorHandler _ err = do
  E.setStatus 500
  E.sendJson { error : Exception.message err }
