module GraphQL.Type.Internal.NullableAndMaybe where

import Prelude

import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Nullable (Nullable, toMaybe, toNullable)
import GraphQL.Type.Internal (Id)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol


-- | NullableAndMaybe
class NullableAndMaybe i o | i -> o, o -> i where
  fromNullableToMaybe :: i -> o
  fromMaybeToNullable :: o -> i

instance nullableAndMaybeInt :: NullableAndMaybe Int Int where
  fromNullableToMaybe = identity
  fromMaybeToNullable = identity

instance nullableAndMaybeNumber :: NullableAndMaybe Number Number where
  fromNullableToMaybe = identity
  fromMaybeToNullable = identity

instance nullableAndMaybeString :: NullableAndMaybe String String where
  fromNullableToMaybe = identity
  fromMaybeToNullable = identity

instance nullableAndMaybeId :: NullableAndMaybe Id Id where
  fromNullableToMaybe = identity
  fromMaybeToNullable = identity

instance nullableAndMaybeBoolean :: NullableAndMaybe Boolean Boolean where
  fromNullableToMaybe = identity
  fromMaybeToNullable = identity

instance nullableAndMaybeArray ::
  ( NullableAndMaybe a b
  ) => NullableAndMaybe (Array a) (Array b)
  where
    fromNullableToMaybe arr = fromNullableToMaybe <$> arr
    fromMaybeToNullable arr = fromMaybeToNullable <$> arr

instance nullableAndMaybeNonEmptyArray ::
  ( NullableAndMaybe a b
  ) => NullableAndMaybe (NonEmpty Array a) (NonEmpty Array b)
  where
    fromNullableToMaybe arr = fromNullableToMaybe <$> arr
    fromMaybeToNullable arr = fromMaybeToNullable <$> arr

instance nullableAndMaybeNullableMaybe ::
  ( NullableAndMaybe a b
  ) => NullableAndMaybe (Nullable a) (Maybe b)
  where
    fromNullableToMaybe na = fromNullableToMaybe <$> toMaybe na
    fromMaybeToNullable mb = toNullable $ fromMaybeToNullable <$> mb

instance nullableAndMaybeRecord ::
  ( RowList.RowToList i iRl
  , RowList.RowToList o oRl
  , FromNullableToMaybeRecord iRl i o
  , FromMaybeToNullableRecord oRl o i
  ) => NullableAndMaybe (Record i) (Record o)
  where
    fromNullableToMaybe ri =
      Builder.build
        ( fromNullableToMaybeRecord
            (RLProxy :: RLProxy iRl)
            ri
        )
        ri
    fromMaybeToNullable ro =
      Builder.build
        ( fromMaybeToNullableRecord
            (RLProxy :: RLProxy oRl)
            ro
        )
      ro

class FromNullableToMaybeRecord (iRl :: RowList) (i :: # Type) (o :: # Type)
  | iRl i -> o
  where
    fromNullableToMaybeRecord :: RLProxy iRl -> Record i -> Builder (Record i) (Record o)

instance nullableToMaybeRecordNil ::
  FromNullableToMaybeRecord RowList.Nil i i
  where
    fromNullableToMaybeRecord _ _ = identity

instance nullableToMaybeRecordCons ::
  ( FromNullableToMaybeRecord restRl i o'
  , Symbol.IsSymbol name
  , NullableAndMaybe a b -- a -> b
  , Row.Cons name a restO o' -- from
  , Row.Cons name b restO o -- to
  ) => FromNullableToMaybeRecord (RowList.Cons name a restRl) i o
  where
    fromNullableToMaybeRecord _ ri
        = Builder.modify
            (SProxy :: SProxy name)
            fromNullableToMaybe
      <<< fromNullableToMaybeRecord
            (RLProxy :: RLProxy restRl)
            ri

class FromMaybeToNullableRecord (oRl :: RowList) (o :: # Type) (i :: # Type)
  | oRl o -> i
  where
    fromMaybeToNullableRecord :: RLProxy oRl -> Record o -> Builder (Record o) (Record i)

instance fromMaybeToNullableRecordNil ::
  FromMaybeToNullableRecord RowList.Nil o o
  where
    fromMaybeToNullableRecord _ _ = identity

instance fromMaybeToNullableRecordCons ::
  ( FromMaybeToNullableRecord restRl o i'
  , Symbol.IsSymbol name
  , NullableAndMaybe a b -- a <- b
  , Row.Cons name b restI i' -- from
  , Row.Cons name a restI i -- to
  ) => FromMaybeToNullableRecord (RowList.Cons name b restRl) o i
  where
    fromMaybeToNullableRecord _ ro
        = Builder.modify
            (SProxy :: SProxy name)
            fromMaybeToNullable
      <<< fromMaybeToNullableRecord
            (RLProxy :: RLProxy restRl)
            ro
