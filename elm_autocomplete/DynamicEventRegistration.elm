module DynamicEventRegistration exposing (..)

import Dict


type alias EventId =
    String


type alias ComponentId =
    String


type Event eventSchema
    = Event eventSchema


type EventType
    = Synchronization
    | Dependency


type alias EventSchema eventType payloadSchema =
    { id : EventId
    , eventType : eventType
    , from : ComponentId

    -- if no receiver (to == Nothing), broadcasting
    , to : Maybe ComponentId

    -- need to have a global contract, e.g. HTTP - Serialization -> String
    -- idea: 1. lenses
    -- 2. no payload is needed, the entire state is passed to update function
    , payload : payloadSchema
    }


type alias State eventSchema componentType =
    { -- dynamically registered events are kept in state
      -- Con: no feedback from compiler, require manual coordination
      events : Dict.Dict EventId (Event eventSchema)
    , components : Dict.Dict ComponentId componentType
    }


type
    ComponentType
    -- the number of component types are finite and static
    = Component1
    | Component2
    | Component3
