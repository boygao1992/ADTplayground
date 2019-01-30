module Test.NullableToMaybeRec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, notNull)
import GraphQL.Type.Internal.NullableAndMaybeRec (fromMaybeToNullableRec, fromNullableToMaybeRec)

-- Test

fromMaybeToNullableRecTest ::
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
fromMaybeToNullableRecTest =
  fromMaybeToNullableRec
    { id: Just 1.0
    , name: Just [ Just 1 ]
    , content: Just { space: Just [ Just { monkey: Just "" } ] }
    }

fromNullableToMaybeRecTest ::
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
fromNullableToMaybeRecTest =
  fromNullableToMaybeRec
    { id: notNull 1.0
    , name: notNull $ [notNull 1]
    , content: notNull { space: notNull [ notNull { monkey: notNull "" } ] }
    }
