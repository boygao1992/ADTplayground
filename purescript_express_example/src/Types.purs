module Types where

import Data.Either (Either)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new) as Ref

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
