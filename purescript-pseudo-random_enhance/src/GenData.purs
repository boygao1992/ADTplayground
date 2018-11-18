module GenData where

import Prelude

import Control.Monad.State (State, evalState, gets, modify_) as SM
import Data.Array (uncons, replicate)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Random.PseudoRandom (Seed, randomR)
import Random.PseudoRandom.WithSeed (randomRsWithSeed)
import Types (Coordinates, Event, EventId, State, Ticket)
import Utils (filterDatedTickets, getAvailableEvents)

type GenDataState =
  { seed :: Seed
  , id :: Int
  }
type GenDataM a = SM.State GenDataState a

initId :: EventId
initId = 0

numEvents :: Int
numEvents = 20

eventNameLength :: Int
eventNameLength = 10

numTickets :: Int
numTickets = 200

minPrice :: Number
minPrice = 0.1

maxPrice :: Number
maxPrice = 1000.0

minCoordinate :: Number
minCoordinate = -10.0

maxCoordinate :: Number
maxCoordinate = 10.0

defaultCoordinates :: Coordinates
defaultCoordinates = { x : 0.0, y : 0.0 }

genData :: Seed -> State
genData initSeed = SM.evalState go initGenDataState
  where
    initGenDataState :: GenDataState
    initGenDataState =
      { seed : initSeed
      , id : initId
      }

    go :: GenDataM State
    go = do
      events <- genEvents numEvents
      tickets <- genTickets numTickets
      let availableTickets = filterDatedTickets events tickets
          availableEvents = getAvailableEvents tickets events
      pure { events, tickets : availableTickets, availableEvents }

    genEvents :: Int -> GenDataM (Array Event)
    genEvents n = traverse (const genEvent) $ replicate n unit

    genEvent :: GenDataM Event
    genEvent = do
      name <- genEventName eventNameLength
      location <- genEventLocation minCoordinate maxCoordinate
      id <- SM.gets _.id
      SM.modify_ _ { id = id + 1 }
      pure { id, name, location }

    genEventName :: Int -> GenDataM String
    genEventName len = do
      seed <- SM.gets _.seed
      let result = randomRsWithSeed 'A' 'Z' len seed
      SM.modify_ _ { seed = result.seed }
      pure $ fromCharArray result.values

    genEventLocation :: Number -> Number -> GenDataM Coordinates
    genEventLocation min max = do
      seed <- SM.gets _.seed
      let result = randomRsWithSeed min max 2 seed
      SM.modify_ _ { seed = result.seed }
      pure $ fromMaybe defaultCoordinates do
        arr1 <- uncons result.values
        arr2 <- uncons arr1.tail
        pure { x : arr1.head , y : arr2.head }

    genTickets :: Int -> GenDataM (Array Ticket)
    genTickets n = traverse (const genTicket) $ replicate n unit

    genTicket :: GenDataM Ticket
    genTicket = do
      id <- SM.gets _.id
      eventId <- genTicketEventId initId (initId + numEvents - 1)
      price <- genTicketPrice minPrice maxPrice
      SM.modify_ _ { id = id + 1 }
      pure { id, eventId, price }

    genTicketEventId :: Int -> Int -> GenDataM EventId
    genTicketEventId min max = do
      seed <- SM.gets _.seed
      let result = randomR min max seed
      SM.modify_ _ { seed = result.newSeed }
      pure result.newVal

    genTicketPrice :: Number -> Number -> GenDataM Number
    genTicketPrice min max = do
      seed <- SM.gets _.seed
      let result = randomR min max seed
      SM.modify_ _ { seed = result.newSeed }
      pure result.newVal
