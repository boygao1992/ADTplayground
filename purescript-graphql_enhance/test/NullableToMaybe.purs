module Test.NullableToMaybe where

import Prelude

import Data.Maybe (Maybe)
import GraphQL.Type.Internal.NullableToMaybe (nullableToMaybe)
import Data.Nullable (Nullable, notNull)

  -- Test

nullableToMaybeTest ::
  { content ::
    { space :: Array
      { monkey :: Maybe String
      }
    }
  , id :: Maybe Number
  , name :: Maybe (Array (Nullable Int)) -- NOTE can only convert one layer of Nullable
  }
nullableToMaybeTest =
  nullableToMaybe
    { id: notNull 1.0
    , name: notNull $ [notNull 1]
    , content: { space: [ { monkey: notNull "" } ] }
    }
