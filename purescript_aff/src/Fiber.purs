module Fiber where

import Prelude

import Data.Maybe (Maybe)
import Data.Map (Map)
import Effect (Effect)

type Unknown = Unit

data Status
  = Suspended -- pending a join
  | Continue -- interpret the next instruction
  | Step_Bind -- apply the next bind
  | Step_Result -- handle potential failure from a result
  | Pending -- an async effect is running
  | Return -- the current stack has returned
  | Completed -- the entire fiber has completed

data Step
  = Bind
  | Pure
  | Sync
  | Async
  | Throw
  | Catch -- enqueue the Catch so that we can call the error handler later on in case of an exception
  | Bracket -- enqueue the Bracket so that we can call the appropriate handlers after resource acquisition
  | Fork
  | Seq

type Fiber =
  { runTick :: Int
  , status :: Status
  , step :: Step
  , fail :: Maybe Unknown
  , interrupt :: Maybe Unknown
  , bhead :: Maybe Unknown
  , btail :: Maybe Unknown
  , attempts :: Maybe (Array Unknown)
  , bracketCount :: Int
  , joinId :: Int
  , joins :: Maybe Unknown
  , rethrow :: Boolean
  }

type Scheduler_State =
  { queue :: Array (Unit -> Effect Unit)
  , limit :: Int
  , size :: Int
  , index :: Int
  , draining :: Boolean
  }

schedular_initialState :: Scheduler_State
schedular_initialState =
  { queue : []
  , limit : 1024
  , size : 0
  , index : 0
  , draining : false
  }

type Supervisor_State =
  { fibers :: Map String Fiber
  }
