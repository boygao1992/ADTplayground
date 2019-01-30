module GraphQL.Type.Internal.NullableAndMaybeRec where

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


-- | NullableAndMaybeRec
class NullableAndMaybeRec i o | i -> o, o -> i where
  fromNullableToMaybeRec :: i -> o
  fromMaybeToNullableRec :: o -> i

instance nullableAndMaybeInt :: NullableAndMaybeRec Int Int where
  fromNullableToMaybeRec = identity
  fromMaybeToNullableRec = identity

instance nullableAndMaybeNumber :: NullableAndMaybeRec Number Number where
  fromNullableToMaybeRec = identity
  fromMaybeToNullableRec = identity

instance nullableAndMaybeString :: NullableAndMaybeRec String String where
  fromNullableToMaybeRec = identity
  fromMaybeToNullableRec = identity

instance nullableAndMaybeId :: NullableAndMaybeRec Id Id where
  fromNullableToMaybeRec = identity
  fromMaybeToNullableRec = identity

instance nullableAndMaybeBoolean :: NullableAndMaybeRec Boolean Boolean where
  fromNullableToMaybeRec = identity
  fromMaybeToNullableRec = identity

instance nullableAndMaybeArray ::
  ( NullableAndMaybeRec a b
  ) => NullableAndMaybeRec (Array a) (Array b)
  where
    fromNullableToMaybeRec arr = fromNullableToMaybeRec <$> arr
    fromMaybeToNullableRec arr = fromMaybeToNullableRec <$> arr

instance nullableAndMaybeNonEmptyArray ::
  ( NullableAndMaybeRec a b
  ) => NullableAndMaybeRec (NonEmpty Array a) (NonEmpty Array b)
  where
    fromNullableToMaybeRec arr = fromNullableToMaybeRec <$> arr
    fromMaybeToNullableRec arr = fromMaybeToNullableRec <$> arr

instance nullableAndMaybeNullableMaybe ::
  ( NullableAndMaybeRec a b
  ) => NullableAndMaybeRec (Nullable a) (Maybe b)
  where
    fromNullableToMaybeRec na = fromNullableToMaybeRec <$> toMaybe na
    fromMaybeToNullableRec mb = toNullable $ fromMaybeToNullableRec <$> mb

instance nullableAndMaybeRecord ::
  ( RowList.RowToList i iRl
  , RowList.RowToList o oRl
  , FromNullableToMaybeRecRecord iRl i o
  , FromMaybeToNullableRecRecord oRl o i
  ) => NullableAndMaybeRec (Record i) (Record o)
  where
    fromNullableToMaybeRec ri =
      Builder.build
        ( fromNullableToMaybeRecRecord
            (RLProxy :: RLProxy iRl)
            ri
        )
        ri
    fromMaybeToNullableRec ro =
      Builder.build
        ( fromMaybeToNullableRecRecord
            (RLProxy :: RLProxy oRl)
            ro
        )
      ro

class FromNullableToMaybeRecRecord (iRl :: RowList) (i :: # Type) (o :: # Type)
  | iRl i -> o
  where
    fromNullableToMaybeRecRecord :: RLProxy iRl -> Record i -> Builder (Record i) (Record o)

instance nullableToMaybeRecordNil ::
  FromNullableToMaybeRecRecord RowList.Nil i i
  where
    fromNullableToMaybeRecRecord _ _ = identity

instance nullableToMaybeRecordCons ::
  ( FromNullableToMaybeRecRecord restRl i o'
  , Symbol.IsSymbol name
  , NullableAndMaybeRec a b -- a -> b
  , Row.Cons name a restO o' -- from
  , Row.Cons name b restO o -- to
  ) => FromNullableToMaybeRecRecord (RowList.Cons name a restRl) i o
  where
    fromNullableToMaybeRecRecord _ ri
        = Builder.modify
            (SProxy :: SProxy name)
            fromNullableToMaybeRec
      <<< fromNullableToMaybeRecRecord
            (RLProxy :: RLProxy restRl)
            ri

class FromMaybeToNullableRecRecord (oRl :: RowList) (o :: # Type) (i :: # Type)
  | oRl o -> i
  where
    fromMaybeToNullableRecRecord :: RLProxy oRl -> Record o -> Builder (Record o) (Record i)

instance fromMaybeToNullableRecRecordNil ::
  FromMaybeToNullableRecRecord RowList.Nil o o
  where
    fromMaybeToNullableRecRecord _ _ = identity

instance fromMaybeToNullableRecRecordCons ::
  ( FromMaybeToNullableRecRecord restRl o i'
  , Symbol.IsSymbol name
  , NullableAndMaybeRec a b -- a <- b
  , Row.Cons name b restI i' -- from
  , Row.Cons name a restI i -- to
  ) => FromMaybeToNullableRecRecord (RowList.Cons name b restRl) o i
  where
    fromMaybeToNullableRecRecord _ ro
        = Builder.modify
            (SProxy :: SProxy name)
            fromMaybeToNullableRec
      <<< fromMaybeToNullableRecRecord
            (RLProxy :: RLProxy restRl)
            ro
