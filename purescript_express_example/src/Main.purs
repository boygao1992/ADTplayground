module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString) as Int
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Node.Express.App (App)
import Node.Express.App (get, post, setProp, use, useOnError, listenHttp) as E
import Node.Process (lookupEnv) as Node

import Types (AppStateRef, initialStateRef)
import Handlers (createTodoHandler, deleteTodoHandler, doneTodoHandler, errorHandler, getLoggerStatusHandler, indexHandler, listTodosHandler, logger, updateTodoHandler)
import ResourceNames as RN

appSetup :: AppStateRef -> App
appSetup stateRef = do
  liftEffect $ Console.log "Setting up"
  E.setProp "json spaces" 4.0
  E.use
    (logger stateRef)
  E.get
    RN.root
    (indexHandler stateRef)
  E.get
    RN.todos
    (listTodosHandler stateRef)
  E.post
    RN.todosCreate
    (createTodoHandler stateRef)
  E.post
    RN.todosUpdate
    (updateTodoHandler stateRef)
  E.post
    RN.todosDelete
    (deleteTodoHandler stateRef)
  E.post
    RN.todosDone
    (doneTodoHandler stateRef)
  E.get
    RN.logger
    (getLoggerStatusHandler stateRef)
  E.useOnError
    (errorHandler stateRef)

main :: Effect Unit
main = do
  stateRef <- initialStateRef

  portEnv <- Node.lookupEnv "Port"
  let port = case portEnv of
              Nothing -> 8080
              Just portL ->
                fromMaybe 8080 $ Int.fromString portL

  _ <- E.listenHttp (appSetup stateRef) port \_ ->
    Console.log $ "Listening on " <> show port
  pure unit
