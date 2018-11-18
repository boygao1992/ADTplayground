module Utils where

import Prelude

import Data.Array (filter, sortBy, take) as A
import Data.Foldable (foldMap)
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.Ord.Min (Min(..))
import Data.Profunctor.Strong ((&&&))
import Data.Set (Set)
import Data.Set (fromFoldable, intersection, member) as Set
import Data.Tuple (Tuple)
import Types (Coordinates, Event, EventId, Ticket)

manhattanDistance :: Coordinates -> Coordinates -> Number
manhattanDistance { x : x1, y : y1 } { x : x2, y : y2 }
  = abs (x1 - x2) + abs (y1 - y2)

filterDatedTickets :: Array Event -> Array Ticket -> Array Ticket
filterDatedTickets es ts =
  A.filter isAvailableTicket ts
  where
    ids1 :: Set EventId
    ids1 = Set.fromFoldable <<< map _.eventId $ ts

    ids2 :: Set EventId
    ids2 = Set.fromFoldable <<< map _.id $ es

    availableEventIds :: Set EventId
    availableEventIds = Set.intersection ids1 ids2

    isAvailableTicket :: Ticket -> Boolean
    isAvailableTicket = (_ `Set.member` availableEventIds) <<< (_.eventId)

getAvailableEvents :: Array Ticket -> Array Event -> Array Event
getAvailableEvents ts es =
  A.filter isAvailableEvent es
  where
    ids1 :: Set EventId
    ids1 = Set.fromFoldable <<< map _.eventId $ ts

    ids2 :: Set EventId
    ids2 = Set.fromFoldable <<< map _.id $ es

    availableEventIds :: Set EventId
    availableEventIds = Set.intersection ids1 ids2

    isAvailableEvent :: Event -> Boolean
    isAvailableEvent = (_ `Set.member` availableEventIds) <<< (_.id)

sortEventsByDistance :: Coordinates -> Array Event -> Array Event
sortEventsByDistance c@{ x, y } = A.sortBy compareDistance
  where
    compareDistance :: Event -> Event -> Ordering
    compareDistance { location : c1 } { location: c2 } =
      compare (manhattanDistance c c1) (manhattanDistance c c2)

getClosestEventIds :: Int -> Coordinates -> Array Event -> Array Event
getClosestEventIds n c events
  | n < 1     = []
  | otherwise = A.take n <<< sortEventsByDistance c $ events

getTicketsByEvent :: Array Ticket -> Event -> Array Ticket
getTicketsByEvent ts { id } = A.filter sameEventId ts
  where
    sameEventId :: Ticket -> Boolean
    sameEventId { eventId } = eventId == id

lowestPrice :: Array Ticket -> Number
lowestPrice = lowestNumber <<< map _.price
  where
    lowestNumber :: Array Number -> Number
    lowestNumber = unwrap <<< foldMap Min

getClosestEventsWithCheapestTicket :: Int -> Coordinates -> Array Event -> Array Ticket -> Array (Tuple Event Number)
getClosestEventsWithCheapestTicket n c es ts =
  map (identity &&& lowestPrice <<< getTicketsByEvent ts)
  $ getClosestEventIds n c es

