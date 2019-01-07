module Generic.RecordToDescriptionRow where

import Data.Generic.Rep (Argument, Constructor)
import Data.Maybe (Maybe)
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Row as Row

class RecordToDescriptionRow rep (desRow :: # Type) | rep -> desRow

instance recordToDescriptionRow ::
  ( RowList.RowToList row rowList
  , RowToDescriptionRow rowList desRowList
  , Row.ListToRow desRowList desRow
  ) => RecordToDescriptionRow (Constructor name (Argument (Record row))) desRow

class RowToDescriptionRow (i :: RowList) (o :: RowList) | i -> o

instance rowToDescriptionRowBaseCase ::
  RowToDescriptionRow RowList.Nil RowList.Nil
else instance rowToDescriptionRowInductionStep ::
  ( RowToDescriptionRow restI restO
  ) => RowToDescriptionRow (RowList.Cons name typ restI) (RowList.Cons name (Maybe String) restO)
