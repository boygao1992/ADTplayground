module Type.Row.Utils where

import Type.IsEqual

import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Type.Data.Boolean as Bool
import Type.Data.Boolean.Utils


-- | HasFieldPred
class HasFieldPred (row :: # Type) (name :: Symbol) (b :: Bool.Boolean) | row name -> b

instance hasFieldPredRowList ::
  ( RowList.RowToList row rl
  , HasFieldPredRowList rl name b
  ) => HasFieldPred row name b

class HasFieldPredRowList (rl :: RowList) (name :: Symbol) (b :: Bool.Boolean) | rl name -> b

instance hasFieldPredRowListBaseCase1 ::
  HasFieldPredRowList RowList.Nil name Bool.False
else instance hasFieldPredRowListBaseCase2 ::
  HasFieldPredRowList (RowList.Cons name typ restRl) name Bool.True
else instance hasFieldPredRowListInductionStep ::
  ( HasFieldPredRowList restRl name b
  ) => HasFieldPredRowList (RowList.Cons name2 typ restRl) name b

-- | IsSubsetPred
-- NOTE hypo < hyper
class IsSubsetPred (hypo :: # Type) (hyper :: # Type) (b :: Bool.Boolean) | hypo hyper -> b

instance isSubsetPredRowList ::
  ( RowList.RowToList hypo hypoRl
  , IsSubsetPredRowList hypoRl hyper b
  ) => IsSubsetPred hypo hyper b

class IsSubsetPredRowList (hypoRl :: RowList) (hyper :: # Type) (b :: Bool.Boolean) | hypoRl hyper -> b

instance isSubsetPredRowListBaseCase ::
  IsSubsetPredRowList RowList.Nil hyper Bool.True
else instance isSubsetPredRowListInductionStep ::
  ( HasFieldPred hyper name b0
  , IsSubsetPredRowListDispatch b0 name typ restRl hyper b
  ) => IsSubsetPredRowList (RowList.Cons name typ restRl) hyper b

class IsSubsetPredRowListDispatch (b0 :: Bool.Boolean) (name :: Symbol) typ (hypoRl :: RowList) (hyper :: # Type) (b :: Bool.Boolean) | b0 name typ hypoRl hyper -> b

instance isSubsetPredRowListDispatchFalse ::
  IsSubsetPredRowListDispatch Bool.False name typ hypoRl hyper Bool.False
else instance isSubsetPredRowListDispatchTrue ::
  ( Row.Cons name typ0 restHyper hyper
  , IsEqual typ0 typ b1
  , IsSubsetPredRowListSecondDispatch b1 hypoRl hyper b
  ) => IsSubsetPredRowListDispatch Bool.True name typ hypoRl hyper b

class IsSubsetPredRowListSecondDispatch (b1 :: Bool.Boolean) (hypoRl :: RowList) (hyper :: # Type) (b :: Bool.Boolean) | b1 hypoRl hyper -> b

instance isSubsetPredRowListSecondDispatchFalse ::
  IsSubsetPredRowListSecondDispatch Bool.False hypoRl hyper Bool.False
else instance isSubsetPredRowListSecondDispatchTrue ::
  ( IsSubsetPredRowList hypoRl hyper b
  ) => IsSubsetPredRowListSecondDispatch Bool.True hypoRl hyper b

-- | IsSubset

class IsSubset (hyper :: # Type) (hypo :: # Type)

instance isSubsetImpl ::
  ( IsSubsetPred hyper hypo b
  , IsTrue b
  ) => IsSubset hyper hypo

