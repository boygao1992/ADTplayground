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

class Relation f a b where
  relation :: f -> a -> b

class RecordableWithRelation f m (r :: # Type)
  where
    toRecordWithRelation :: f -> m -> Maybe (Record r)

instance recordableWithRelationImpl ::
  ( RowList.RowToList r rl
  , RecordableWithRelationList f m rl r
  ) => RecordableWithRelation f m r
  where
    toRecordWithRelation f t =
      Builder.build <@> {}
      <$> toRecordWithRelationList (RLProxy :: RLProxy rl) f t

class RecordableWithRelationList f m (rl :: RowList) (to :: # Type)| f rl -> to
  where
    toRecordWithRelationList :: RLProxy rl -> f -> m -> Maybe (Builder {} { | to })

instance recordableWithRelationListNil ::
  RecordableWithRelationList f m RowList.Nil ()
  where
    toRecordWithRelationList _ _ _ = Just identity

instance recordableWithRelationListConsOptional ::
  ( Symbol.IsSymbol name
  , Index m String source
  , Relation f source (Maybe target)
  , Row.Cons name (Maybe target) restTo to
  , Row.Lacks name restTo
  , RecordableWithRelationList f m restRl restTo
  ) => RecordableWithRelationList f m (RowList.Cons name (Maybe target) restRl) to
  where
    toRecordWithRelationList _ f t =
      ( Builder.insert
          (SProxy :: SProxy name)
          ( relation f
            =<< preview (ix (Symbol.reflectSymbol (SProxy :: SProxy name))) t)
        <<< _
      )
      <$> toRecordWithRelationList (RLProxy :: RLProxy restRl) f t
else
instance recordableWithRelationListConsRequired ::
  ( Symbol.IsSymbol name
  , Index m String source
  , Relation f source (Maybe target)
  , Row.Cons name target restTo to
  , Row.Lacks name restTo
  , RecordableWithRelationList f m restRl restTo
  ) => RecordableWithRelationList f m (RowList.Cons name target restRl) to
  where
    toRecordWithRelationList _ f t =
      case relation f
           =<< preview (ix (Symbol.reflectSymbol (SProxy :: SProxy name))) t
      of
        Nothing ->
          const Nothing
        Just val ->
          Just <<< (Builder.insert (SProxy :: SProxy name) val <<< _)
      =<< toRecordWithRelationList (RLProxy :: RLProxy restRl) f t

