module Types where

import Prelude
import Random.PseudoRandom (Seed)

type EventId = Int
type TicketId = Int

type Event =
  { id :: EventId
  , name :: String
  , location :: Coordinates
  }

type Coordinates =
  { x :: Number
  , y :: Number
  }

type Ticket =
  { id :: TicketId
  , eventId :: EventId
  , price :: Number
  }

type State =
  { events :: Array Event
  , tickets :: Array Ticket
  , availableEvents :: Array Event -- Cache
  }

type AppState =
  { state :: State
  , seed :: Seed
  }

initialState :: State
initialState =
  { events :
      [ { id : 0, name : "A", location : { x : 0.0, y : 0.0 } }
      , { id : 1, name : "B", location : { x : -10.0, y : -10.0 } }
      , { id : 2, name : "C", location : { x : 10.0, y : 10.0 } }
      , { id : 3, name : "D", location : { x : 1.0, y : 1.0 } }
      , { id : 4, name : "E", location : { x : 2.0, y : 2.0 } }
      , { id : 5, name : "F", location : { x : 3.0, y : 3.0 } }
      ]
  , tickets :
      [ { id : 0, eventId : 0, price : 10.0 }
      , { id : 1, eventId : 0, price : 80.0 }
      , { id : 2, eventId : 0, price : 90.0 }
      , { id : 3, eventId : 0, price : 70.0 }
      , { id : 4, eventId : 0, price : 5.0 }
      , { id : 5, eventId : 1, price : 10.0 }
      , { id : 6, eventId : 1, price : 80.0 }
      , { id : 7, eventId : 1, price : 90.0 }
      , { id : 8, eventId : 1, price : 70.0 }
      , { id : 9, eventId : 1, price : 20.0 }
      , { id : 10, eventId : 2, price : 10.0 }
      , { id : 11, eventId : 2, price : 80.0 }
      , { id : 12, eventId : 2, price : 90.0 }
      , { id : 13, eventId : 2, price : 70.0 }
      , { id : 14, eventId : 2, price : 20.0 }
      , { id : 15, eventId : 3, price : 10.0 }
      , { id : 16, eventId : 3, price : 80.0 }
      , { id : 17, eventId : 3, price : 90.0 }
      , { id : 18, eventId : 3, price : 70.0 }
      , { id : 19, eventId : 3, price : 20.0 }
      , { id : 20, eventId : 4, price : 10.0 }
      , { id : 21, eventId : 4, price : 80.0 }
      , { id : 22, eventId : 4, price : 90.0 }
      , { id : 23, eventId : 4, price : 70.0 }
      , { id : 24, eventId : 4, price : 20.0 }
      , { id : 25, eventId : 5, price : 10.0 }
      , { id : 26, eventId : 5, price : 80.0 }
      , { id : 27, eventId : 5, price : 90.0 }
      , { id : 28, eventId : 5, price : 70.0 }
      , { id : 29, eventId : 5, price : 20.0 }
      ]
  , availableEvents : []
  }
