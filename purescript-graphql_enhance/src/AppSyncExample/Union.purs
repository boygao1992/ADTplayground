module AppSyncExample.Union where

import Data.Maybe (Maybe)

newtype ID = ID String

type Venue =
  { id :: ID
  , name :: String
  , address :: Maybe String
  , maxOccupancy :: Maybe Int
  }

type Event =
  ( id :: ID
  , name :: String
  , startsAt :: Maybe String
  , endsAt :: Maybe String
  , venue :: Maybe Venue
  , minAgeRestriction :: Maybe Int
  )

type Festival =
  { performers :: Array String -- [] ~ Nothing ~ null
  | Event
  }

type Conference =
  { speakers :: Array String
  , workshops :: Array String
  | Event
  }

type Concert =
  { performingBand :: Maybe String
  | Event
  }

data SearchResult
  = SR_Conference Conference
  | SR_Festival Festival
  | SR_Concert Concert
  | SR_Venue Venue
