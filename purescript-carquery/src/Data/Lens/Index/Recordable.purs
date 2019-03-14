module Data.Lens.Index.Recordable where

import Prelude

import Data.Lens (preview)
import Data.Lens.Index (class Index, ix)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Read (class Read, class Empty, read, fill, empty)
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
    toRecord :: m -> Record r

instance recordableImpl ::
  ( RowList.RowToList r rl
  , RecordableList m rl r
  ) => Recordable m r
  where
    toRecord t =
      Builder.build
        ( toRecordList
            (RLProxy :: RLProxy rl)
            t
        )
        {}

class RecordableList m (rl :: RowList) (to :: # Type)| rl -> to
  where
    toRecordList :: RLProxy rl -> m -> Builder {} { | to }

instance recordableListNil ::
  RecordableList m RowList.Nil ()
  where
    toRecordList _ _ = identity

instance recordableListConsOptional ::
  ( Index m String (Maybe String)
  , Symbol.IsSymbol name
  , Read typ
  , Row.Cons name (Maybe typ) restTo to
  , Row.Lacks name restTo
  , RecordableList m restRl restTo
  ) => RecordableList m (RowList.Cons name (Maybe typ) restRl) to
  where
    toRecordList _ t =
          ( Builder.insert
              (SProxy :: SProxy name)
              ( case preview (ix (Symbol.reflectSymbol (SProxy :: SProxy name))) t of
                   Just str -> str >>= read
                   Nothing -> Nothing
              )
          )
      <<< ( toRecordList
              (RLProxy :: RLProxy restRl)
              t
          )
else instance readableListConsRequired ::
  ( Index m String (Maybe String)
  , Symbol.IsSymbol name
  , Read typ
  , Empty typ
  , Row.Cons name typ restTo to
  , Row.Lacks name restTo
  , RecordableList m restRl restTo
  ) => RecordableList m (RowList.Cons name typ restRl) to
  where
    toRecordList _ t =
          ( Builder.insert
              (SProxy :: SProxy name)
              ( maybe empty (maybe empty fill)
                $ preview (ix (Symbol.reflectSymbol (SProxy :: SProxy name))) t
              )
          )
      <<< ( toRecordList
              (RLProxy :: RLProxy restRl)
              t
          )
