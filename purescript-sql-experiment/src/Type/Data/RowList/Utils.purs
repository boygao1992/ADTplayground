module Type.Data.RowList.Utils where

import Prim.RowList (Nil, kind RowList)
import Type.Data.Boolean as Bool

class IsEmptyPred (rl :: RowList) (b :: Bool.Boolean) | rl -> b

instance isEmptyPredNil :: IsEmptyPred Nil Bool.True
else instance isEmptyPredCons :: IsEmptyPred other Bool.False
