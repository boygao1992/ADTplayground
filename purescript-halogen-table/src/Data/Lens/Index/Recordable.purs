module Data.Lens.Index.Recordable where

import Prelude

import Data.Lens (preview)
import Data.Lens.Index (class Index, ix)
import Data.Maybe (Maybe(..))
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy (..))
import Type.Data.Symbol (SProxy (..))
import Type.Data.Symbol as Symbol
import Heterogeneous.Mapping (class Mapping, mapping)

class Recordable m (r :: # Type)
  where
    toRecord :: m -> Maybe (Record r)

instance recordableImpl ::
  ( RowList.RowToList r rl
  , RecordableList m rl r
  ) => Recordable m r
  where
    toRecord t =
      Builder.build <@> {}
      <$> toRecordList (RLProxy :: RLProxy rl) t

class RecordableList m (rl :: RowList) (to :: # Type)| rl -> to
  where
    toRecordList :: RLProxy rl -> m -> Maybe (Builder {} { | to })

instance recordableListNil ::
  RecordableList m RowList.Nil ()
  where
    toRecordList _ _ = Just identity

instance recordableListConsOptional ::
  ( Symbol.IsSymbol name
  , Index m String typ
  , Row.Cons name (Maybe typ) restTo to
  , Row.Lacks name restTo
  , RecordableList m restRl restTo
  ) => RecordableList m (RowList.Cons name (Maybe typ) restRl) to
  where
    toRecordList _ t =
      ( Builder.insert
          (SProxy :: SProxy name)
          (preview (ix (Symbol.reflectSymbol (SProxy :: SProxy name))) t)
        <<< _
      )
      <$> toRecordList (RLProxy :: RLProxy restRl) t
else
instance recordableListConsRequired ::
  ( Symbol.IsSymbol name
  , Index m String typ
  , Row.Cons name typ restTo to
  , Row.Lacks name restTo
  , RecordableList m restRl restTo
  ) => RecordableList m (RowList.Cons name typ restRl) to
  where
    toRecordList _ t =
      case preview (ix (Symbol.reflectSymbol (SProxy :: SProxy name))) t of
        Nothing ->
          const Nothing
        Just val ->
          Just <<< (Builder.insert (SProxy :: SProxy name) val <<< _)
      =<< toRecordList (RLProxy :: RLProxy restRl) t

------------------------
-- RecordableWithMapping

class RecordableWithMapping f m (r :: # Type)
  where
    toRecordWithMapping :: f -> m -> Maybe (Record r)

instance recordableWithMappingImpl ::
  ( RowList.RowToList r rl
  , RecordableWithMappingList f m rl r
  ) => RecordableWithMapping f m r
  where
    toRecordWithMapping f t =
      Builder.build <@> {}
      <$> toRecordWithMappingList (RLProxy :: RLProxy rl) f t

class RecordableWithMappingList f m (rl :: RowList) (to :: # Type)| f rl -> to
  where
    toRecordWithMappingList :: RLProxy rl -> f -> m -> Maybe (Builder {} { | to })

instance recordableWithMappingListNil ::
  RecordableWithMappingList f m RowList.Nil ()
  where
    toRecordWithMappingList _ _ _ = Just identity

instance recordableWithMappingListConsOptional ::
  ( Symbol.IsSymbol name
  , Index m String source
  , Mapping f source target
  , Row.Cons name (Maybe target) restTo to
  , Row.Lacks name restTo
  , RecordableWithMappingList f m restRl restTo
  ) => RecordableWithMappingList f m (RowList.Cons name (Maybe target) restRl) to
  where
    toRecordWithMappingList _ f t =
      ( Builder.insert
          (SProxy :: SProxy name)
          ( mapping f
            <$> preview (ix (Symbol.reflectSymbol (SProxy :: SProxy name))) t)
        <<< _
      )
      <$> toRecordWithMappingList (RLProxy :: RLProxy restRl) f t
else
instance recordableWithMappingListConsRequired ::
  ( Symbol.IsSymbol name
  , Index m String source
  , Mapping f source target
  , Row.Cons name target restTo to
  , Row.Lacks name restTo
  , RecordableWithMappingList f m restRl restTo
  ) => RecordableWithMappingList f m (RowList.Cons name target restRl) to
  where
    toRecordWithMappingList _ f t =
      case preview (ix (Symbol.reflectSymbol (SProxy :: SProxy name))) t of
        Nothing ->
          const Nothing
        Just val ->
          Just <<< (Builder.insert (SProxy :: SProxy name) (mapping f val) <<< _)
      =<< toRecordWithMappingList (RLProxy :: RLProxy restRl) f t
