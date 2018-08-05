module DynamicEventRegistration exposing (..)

import Dict


type alias EventId =
    String


type alias ComponentId =
    String


type EventType
    = Input
    | Dependency
    | Output


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


type alias StateSchema eventSchema componentType =
    { -- dynamically registered events are kept in state
      -- Con: no feedback from compiler, require manual coordination
      events : Dict.Dict EventId eventSchema
    , components : Dict.Dict ComponentId componentType
    }


type
    ComponentType
    -- the number of component types are finite and static
    = Component1 -- Component1.model
    | Component2 -- Component2.model
    | Component3 -- Component3.model


type alias Event payloadSchema =
    EventSchema EventType payloadSchema


type alias State payloadSchema =
    StateSchema (Event payloadSchema) ComponentType


stateTransition : Event payloadSchema -> State payloadSchema -> ( State payloadSchema, Event payloadSchema )
