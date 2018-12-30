module AppSyncExample.Interface where

import Data.Maybe (Maybe)

-- polymorphism through interface in GraphQL is equivalent to
-- | bundling uncommon fields in subtypes into a tagged union

newtype ID = ID String

type Venue =
  { id :: ID
  , name :: String
  , address :: Maybe String
  , maxOccupancy :: Maybe Int
  }

type Event =
  { id :: ID
  , name :: String
  , startsAt :: Maybe String
  , endsAt :: Maybe String
  , venue :: Maybe Venue
  , minAgeRestriction :: Maybe Int
  , category :: EventCategory
  }

data EventCategory
  = Festival { performers :: Array String }
  | Conference { spearkers :: Array String, workshops :: Array String }
  | Concert { performingBand :: Maybe String }
