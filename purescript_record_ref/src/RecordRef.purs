module RecordRef where

import Prelude

import Data.Array ((:))
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Effect (Effect)
import Prim.Row as Row
import Type.Data.Boolean as Bool
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))

foreign import data RecordRef :: # Type -> Type

foreign import new :: forall row. (Record row) -> Effect (RecordRef row)

foreign import read :: forall row. RecordRef row -> Effect (Record row)

foreign import _modify' :: forall row b.
  Fn2 (Record row -> { state :: Record row, value :: b}) (RecordRef row) (Effect b)

modify'
  :: forall row b
   . (Record row -> { state :: Record row, value :: b})
  -> RecordRef row
  -> Effect b
modify' = runFn2 _modify'

modify
  :: forall row
   . (Record row -> Record row)
  -> RecordRef row
  -> Effect (Record row)
modify f = modify'
             \rec ->
               let
                 rec' = f rec
               in
                 { state : rec', value : rec' }

modify_
  :: forall row
  . (Record row -> Record row)
  -> RecordRef row
  -> Effect Unit
modify_ f ref = void $ modify f ref

foreign import _write :: forall row.
  Fn2 (Record row) (RecordRef row) (Effect Unit)

write :: forall row. Record row -> RecordRef row -> Effect Unit
write = runFn2 _write


foreign import _pathRead
  :: forall row typ
   . Fn2
      (Array String)
      (RecordRef row)
      (Effect typ)

pathRead
  :: forall row pl typ
   . RowPathAccess row pl typ
  => PListToArray pl
  => PLProxy pl
  -> RecordRef row
  -> Effect typ
pathRead plist = runFn2 _pathRead (pListToArray plist)

foreign import _pathModify'
  :: forall row typ b
   . Fn3
       (Array String)
       (typ -> { state :: typ, value :: b })
       (RecordRef row)
       (Effect b)

pathModify'
  :: forall row pl typ b
   . RowPathAccess row pl typ
  => PListToArray pl
  => PLProxy pl
  -> (typ -> { state :: typ, value :: b })
  -> RecordRef row
  -> Effect b
pathModify' plist = runFn3 _pathModify' (pListToArray plist)

pathModify
  :: forall row pl typ
   . RowPathAccess row pl typ
  => PListToArray pl
  => PLProxy pl
  -> (typ -> typ)
  -> RecordRef row
  -> Effect typ
pathModify plist f = pathModify'
                      plist
                      \rec ->
                        let
                          rec' = f rec
                        in
                          { state : rec', value : rec'}

pathModify_
  :: forall row pl typ
   . RowPathAccess row pl typ
  => PListToArray pl
  => PLProxy pl
  -> (typ -> typ)
  -> RecordRef row
  -> Effect Unit
pathModify_ plist f ref = void $ pathModify plist f ref

foreign import _pathWrite
  :: forall row typ
   . Fn3
       (Array String)
       typ
       (RecordRef row)
       (Effect Unit)

pathWrite
  :: forall row pl typ
   . RowPathAccess row pl typ
  => PListToArray pl
  => PLProxy pl
  -> typ
  -> RecordRef row
  -> Effect Unit
pathWrite plist = runFn3 _pathWrite (pListToArray plist)

-- | PList, hold a path (Array String) in the type system
foreign import kind PList
foreign import data PCons :: Symbol -> PList -> PList
foreign import data PNil :: PList

-- | ValidPathPred
class ValidPathPred (row :: # Type) (pl :: PList) (b :: Bool.Boolean) | row pl -> b

instance validPathPredBaseCase ::
  ( Row.Cons name typ restRow row
  ) => ValidPathPred row (PCons name PNil) Bool.True
else instance validPathPredInductionStep ::
  ( Row.Cons name (Record childRow) restRow row
  , ValidPathPred childRow restPl b
  ) => ValidPathPred row (PCons name restPl) b
else instance validPathInvalid ::
  ValidPathPred row pl Bool.False

-- | RowPathAccess
class RowPathAccess (row :: # Type) (pl :: PList) typ | row pl -> typ

instance rowPathAccesBaseCase1 ::
  RowPathAccess row PNil (Record row)
else instance rowPathAccessBaseCase2 ::
  ( Row.Cons name typ restRow row
  ) => RowPathAccess row (PCons name PNil) typ
else instance rowPathAccessInductionStep ::
  ( Row.Cons name (Record childRow) restRow row
  , RowPathAccess childRow restPl typ
  ) => RowPathAccess row (PCons name restPl) typ
-- TODO better error message

-- | PListToArray
class PListToArray (pl :: PList) where
  pListToArray :: PLProxy pl -> Array String

instance pListToArrayBaseCase :: PListToArray PNil where
  pListToArray _ = []
else instance pListToArrayInductionStep ::
  ( Symbol.IsSymbol name
  , PListToArray restPl
  ) => PListToArray (PCons name restPl)
  where
    pListToArray _
     = Symbol.reflectSymbol (SProxy :: SProxy name)
     : pListToArray (PLProxy :: PLProxy restPl)


-- | Test
data PLProxy (pl :: PList) = PLProxy

-- class ValidPathPred
validPathPred :: forall row pl b. ValidPathPred row pl b => RProxy row -> PLProxy pl -> Bool.BProxy b
validPathPred _ _ = Bool.BProxy :: Bool.BProxy b

validPathPredExample :: Bool.BProxy Bool.True
validPathPredExample = validPathPred
                       (RProxy :: RProxy ( a :: { b :: Int }, c :: String ))
                       (PLProxy :: PLProxy (PCons "a" (PCons "b" PNil)))

-- class RowPathAccess
rowPathAccess :: forall row pl typ. RowPathAccess row pl typ => RProxy row -> PLProxy pl -> Proxy typ
rowPathAccess _ _ = Proxy :: Proxy typ

rowPathAccessExample :: Proxy Int
rowPathAccessExample = rowPathAccess
                       (RProxy :: RProxy ( a :: { b :: Int }, c :: String ))
                       (PLProxy :: PLProxy (PCons "a" (PCons "b" PNil)))
