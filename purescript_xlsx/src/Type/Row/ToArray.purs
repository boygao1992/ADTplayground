module Type.Row.ToArray where

import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Row as Row
import Type.Row (RProxy(..))


class ToArray (i :: # Type) (o :: # Type) | i -> o
instance toArrayImpl ::
  ( RowList.RowToList i iRl
  , ToArrayRowList iRl oRl
  , Row.ListToRow oRl o
  ) => ToArray i o

class ToArrayRowList (i :: RowList) (o :: RowList) | i -> o

instance toArrayRowListNil ::
  ToArrayRowList RowList.Nil RowList.Nil

instance toArrayRowListCons ::
  ( ToArrayRowList restIRl restORl
  ) => ToArrayRowList (RowList.Cons label typ restIRl) (RowList.Cons label (Array typ) restORl)

toArray :: forall i o. ToArray i o => RProxy i -> RProxy o
toArray _ = RProxy :: RProxy o

toArrayTest :: RProxy
  ( "Duty" :: Array Int
  , "EAN" :: Array String
  , "Product Name" :: Array String
  , "SKU#" :: Array String
  )
toArrayTest = toArray (RProxy :: RProxy
                                 ( "SKU#" :: String
                                 , "Product Name" :: String
                                 , "EAN" :: String
                                 , "Duty" :: Int
                                 )
                      )
