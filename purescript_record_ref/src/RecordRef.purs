module RecordRef where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Prim.Row as Row
import Type.Row (RProxy(..))
import Type.Data.Boolean as Bool

foreign import data RecordRef :: # Type -> Type

foreign import new :: forall row. { | row } -> Effect (RecordRef row)

foreign import read :: forall row. RecordRef row -> Effect { | row }

foreign import _modify' :: forall row b.
  Fn2 ({ | row} -> { state :: { | row}, value :: b}) (RecordRef row) (Effect b)

modify'
  :: forall row b
   . ({ | row} -> { state :: { | row}, value :: b})
  -> RecordRef row
  -> Effect b
modify' = runFn2 _modify'

foreign import _write :: forall row.
  Fn2 { | row} (RecordRef row) (Effect Unit)

write :: forall row. { | row } -> RecordRef row -> Effect Unit
write = runFn2 _write

foreign import kind PList
foreign import data PCons :: Symbol -> PList -> PList
foreign import data PNil :: PList

class ValidPathPred (row :: # Type) (pl :: PList) (b :: Bool.Boolean) | row pl -> b

instance validPathPredBaseCase1 :: ValidPathPred row PNil Bool.True
else instance validPathPredBaseCase2 ::
  ( Row.Cons name typ restRow row
  ) => ValidPathPred row (PCons name PNil) Bool.True
else instance validPathPredInductionStep ::
  ( Row.Cons name (Record childRow) restRow row
  , ValidPathPred childRow restPl b
  ) => ValidPathPred row (PCons name restPl) b
else instance validPathInvalid ::
  ValidPathPred row pl Bool.False

-- | Test
data PLProxy (pl :: PList) = PLProxy

validPathPred :: forall row pl b. ValidPathPred row pl b => RProxy row -> PLProxy pl -> Bool.BProxy b
validPathPred _ _ = Bool.BProxy :: Bool.BProxy b

validPathPredExample :: Bool.BProxy Bool.True
validPathPredExample = validPathPred
                       (RProxy :: RProxy ( a :: { b :: Int }, c :: String ))
                       (PLProxy :: PLProxy (PCons "a" (PCons "b" PNil)))
