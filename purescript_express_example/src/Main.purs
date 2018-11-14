module Main where

import Prelude

import Data.Array (deleteAt, length, mapWithIndex, modifyAt, snoc) as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString) as Int
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Effect.Exception (Error)
import Effect.Exception (message, error) as Exception
import Effect.Ref (Ref)
import Effect.Ref (new, read, modify') as Ref
import Node.Express.App (App)
import Node.Express.App (get, setProp, use, useOnError, listenHttp) as E
import Node.Express.Handler (Handler)
import Node.Express.Handler (next) as E
import Node.Express.Request (getOriginalUrl, getQueryParam, getRouteParam, getUserData, setUserData) as E
import Node.Express.Response (sendJson, setStatus) as E
-- import Node.HTTP (Server)
import Node.Process (lookupEnv) as Node

-- Types

-- | Data Models

type Todo =
  { description :: String
  , isDone :: Boolean
  }

type TodoId = Int

-- | App Global
type AppState = Array Todo

initialState :: AppState
initialState = []

type AppStateRef = Ref AppState

initialStateRef :: Effect AppStateRef
initialStateRef = Ref.new initialState

-- | Response

type AppError = String

type Result a =
  { state :: AppState
  , value :: Either AppError a
  }

type IndexedTodo =
  { id :: TodoId
  , description :: String
  , isDone :: Boolean
  }

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

-- Handlers
logger :: AppStateRef -> Handler
logger stateRef = do
  state <- liftEffect $ Ref.read stateRef
  url <- E.getOriginalUrl
  liftEffect $ Console.log $ ">>> " <> url <> " count =" <> (show $ A.length state)
  E.setUserData "logged" url
  E.next

errorHandler :: AppStateRef -> Error -> Handler
errorHandler _ err = do
  E.setStatus 400
  E.sendJson { error : Exception.message err }

indexHandler :: AppStateRef -> Handler
indexHandler _ = do
  E.sendJson help
  where
    help =
      { name : "PureScript-E Example"
      , endpoints :
        { listTodos : "/list"
        , createTodo : "/create?desc=Do+something"
        , doneTodo : "/done/:id"
        , updateTodo : "/update/:id?desc=Do+something+else"
        , deleteTodo : "/delete/:id"
        }
      }

getLoggerStatusHandler :: AppStateRef -> Handler
getLoggerStatusHandler _ = do
  userdata <- E.getUserData "logged"
  E.sendJson $ fromMaybe "missing" userdata

listTodosHandler :: AppStateRef -> Handler
listTodosHandler stateRef = do
  state <- liftEffect $ Ref.read stateRef
  E.sendJson $ getTodoWithIndexes state

-- /create?desc=Do+something
createTodoHandler :: AppStateRef -> Handler
createTodoHandler stateRef = do
  descParam <- E.getQueryParam "desc"
  case descParam of
    Nothing ->
      errorHandler stateRef (Exception.error "Description is required.")
    Just desc -> do
      newId <- liftEffect
        $ Ref.modify' (addTodo { description: desc, isDone: false}) stateRef
      E.sendJson { status: "Created", id: newId}

-- /update/:id?desc=Do+something+else
updateTodoHandler :: AppStateRef -> Handler
updateTodoHandler stateRef = do
  idParam <- E.getRouteParam "id"
  descParam <- E.getQueryParam "desc"
  case idParam, descParam of
    Nothing, Nothing ->
      errorHandler stateRef (Exception.error "Both Id and Description are required.")
    Nothing, Just _ -> errorHandler stateRef (Exception.error "Id is required.")
    Just _, Nothing -> errorHandler stateRef (Exception.error "Description is required.")
    Just idLiteral, Just desc ->
      case Int.fromString idLiteral of
        Nothing -> errorHandler stateRef (Exception.error "Id must be an integer.")
        Just id -> do
          res <- liftEffect $ Ref.modify' (deleteTodo id) stateRef
          case res of
            Left msg -> errorHandler stateRef (Exception.error msg)
            Right _ -> E.sendJson { status : "Updated" }

-- /delete/:id
deleteTodoHandler :: AppStateRef -> Handler
deleteTodoHandler stateRef = do
  idParam <- E.getRouteParam "id"
  case idParam of
    Nothing -> errorHandler stateRef (Exception.error "Id is required.")
    Just idLiteral ->
      case Int.fromString idLiteral of
        Nothing -> errorHandler stateRef (Exception.error "Id must be an integer.")
        Just id -> do
          res <- liftEffect $ Ref.modify' (deleteTodo id) stateRef
          case res of
            Left msg -> errorHandler stateRef (Exception.error msg)
            _ -> E.sendJson {status: "Deleted"}

doneTodoHandler :: AppStateRef -> Handler
doneTodoHandler stateRef = do
  idParam <- E.getRouteParam "id"
  case idParam of
    Nothing -> errorHandler stateRef (Exception.error "Id is required.")
    Just idLiteral ->
      case Int.fromString idLiteral of
        Nothing -> errorHandler stateRef (Exception.error "Id must be an integer.")
        Just id -> do
          res <- liftEffect $ Ref.modify' (doneTodo id) stateRef
          case res of
            Left msg -> errorHandler stateRef (Exception.error msg)
            _ -> E.sendJson {status: "Done"}

appSetup :: AppStateRef -> App
appSetup stateRef = do
  liftEffect $ Console.log "Setting up"
  E.setProp "json spaces" 4.0
  E.use               (logger                   stateRef)
  E.get "/"           (indexHandler             stateRef)
  E.get "/list"       (listTodosHandler         stateRef)
  E.get "/create"     (createTodoHandler        stateRef)
  E.get "/update/:id" (updateTodoHandler        stateRef)
  E.get "/delete/:id" (deleteTodoHandler        stateRef)
  E.get "/done/:id"   (doneTodoHandler          stateRef)
  E.get "/logger"     (getLoggerStatusHandler   stateRef)
  E.useOnError        (errorHandler             stateRef)


main :: Effect Unit
main = do
  stateRef <- initialStateRef

  portEnv <- Node.lookupEnv "Port"
  let port = case portEnv of
              Nothing -> 8080
              Just portL ->
                case Int.fromString portL of
                  Nothing -> 8080
                  Just port -> port

  _ <- E.listenHttp (appSetup stateRef) port \_ ->
    Console.log $ "Listening on " <> show port
  pure unit
