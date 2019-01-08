module RowList.Utils where

import Prim.RowList (kind RowList)
import Prim.RowList as RowList

class ReverseRowList (i :: RowList) (o :: RowList) | i -> o

instance reverseRowListImpl ::
  ( ReverseRowListImpl RowList.Nil i o
  ) => ReverseRowList i o

class ReverseRowListImpl (acc :: RowList) (i :: RowList) (o :: RowList) | i -> o

instance reverseRowListImplBaseCase ::
  ReverseRowListImpl acc RowList.Nil acc
else instance reverseRowListImplInductionStep ::
  ( ReverseRowListImpl (RowList.Cons name typ acc) restI o
  ) => ReverseRowListImpl acc (RowList.Cons name typ restI) o

