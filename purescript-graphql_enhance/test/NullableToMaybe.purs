module Test.NullableToMaybe where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, notNull)
import GraphQL.Type.Internal.NullableToMaybe (maybeToNullable, nullableToMaybe)

  -- Test

nullableToMaybeTest ::
  { content ::
      Maybe
        { space ::
            Maybe
              ( Array
                  ( Maybe
                      { monkey :: Maybe String
                      }
                  )
              )
        }
  , id :: Maybe Number
  , name :: Maybe (Array (Maybe Int))
  }
nullableToMaybeTest =
  nullableToMaybe
    { id: notNull 1.0
    , name: notNull $ [notNull 1]
    , content: notNull { space: notNull [ notNull { monkey: notNull "" } ] }
    }

maybeToNullableTest ::
  { content ::
      Nullable
        { space ::
            Nullable
              ( Array
                  ( Nullable
                    { monkey :: Nullable String
                    }
                  )
              )
        }
  , id :: Nullable Number
  , name :: Nullable (Array (Nullable Int))
  }
maybeToNullableTest =
  maybeToNullable
    { id: Just 1.0
    , name: Just [ Just 1 ]
    , content: Just { space: Just [ Just { monkey: Just "" } ] }
    }
