module GraphQL.Type.Internal.NullableAndMaybe where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)

class NullableAndMaybe i o | i -> o, o -> i where
  fromNullableToMaybe :: i -> o
  fromMaybeToNullable :: o -> i

instance nullableAndMaybeBaseCase ::
  NullableAndMaybe (Nullable a) (Maybe a)
  where
    fromNullableToMaybe = toMaybe
    fromMaybeToNullable = toNullable
else instance nullableAndMaybeOther ::
  NullableAndMaybe a a where
    fromNullableToMaybe = identity
    fromMaybeToNullable = identity
