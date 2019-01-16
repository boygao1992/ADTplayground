module Record.ST.Nested where

import Prelude

import Control.Monad.ST (ST)
import Data.Array ((:))
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Prim.Row as Row
import Record.ST (STRecord)
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

-- | PList, hold a path (Array String) in the type system

foreign import kind PList
foreign import data PCons :: Symbol -> PList -> PList
foreign import data PNil :: PList

data PLProxy (pl :: PList) = PLProxy

-- | ValidPathPred
-- class ValidPathPred (row :: # Type) (pl :: PList) (b :: Bool.Boolean) | row pl -> b

-- instance validPathPredBaseCase ::
--   ( Row.Cons name typ restRow row
--   ) => ValidPathPred row (PCons name PNil) Bool.True
-- else instance validPathPredInductionStep ::
--   ( Row.Cons name (Record childRow) restRow row
--   , ValidPathPred childRow restPl b
--   ) => ValidPathPred row (PCons name restPl) b
-- else instance validPathInvalid ::
--   ValidPathPred row pl Bool.False

-- | RowPListAccess
class RowPListAccess (row :: # Type) (pl :: PList) typ | row pl -> typ

-- instance rowPListAccesBaseCase1 :: -- TODO Error: empty path
--   RowPListAccess row PNil (Record row)
-- else 
instance rowPListAccessBaseCase2 ::
  ( Row.Cons name typ restRow row
  ) => RowPListAccess row (PCons name PNil) typ
else instance rowPListAccessInductionStep ::
  ( Row.Cons name (Record childRow) restRow row
  , RowPListAccess childRow restPl typ
  ) => RowPListAccess row (PCons name restPl) typ
-- TODO better error message

-- | RowPListAccess
class RowPListAccessRecord (row :: # Type) (pl :: PList) (childRow :: # Type) | row pl -> childRow

-- instance rowPListAccesBaseCase1 :: -- TODO Error: empty path
--   RowPListAccess row PNil (Record row)
instance rowPListAccessRecordBaseCase2 ::
  ( Row.Cons name (Record childRow) restRow row
  ) => RowPListAccessRecord row (PCons name PNil) childRow
else instance rowPListAccessRecordInductionStep ::
  ( Row.Cons name (Record childRow) restRow row
  , RowPListAccessRecord childRow restPl outputRow
  ) => RowPListAccessRecord row (PCons name restPl) outputRow
-- TODO better error message

-- | ReversePList
class ReversePList (pl :: PList) (reversed :: PList) | pl -> reversed

instance reversePListImpl ::
  ( ReversePListImpl PNil pl reversed
  ) => ReversePList pl reversed

class ReversePListImpl (currentPl :: PList) (restPl :: PList) (result :: PList) | currentPl restPl -> result

instance reversePListImplBaseCase ::
  ReversePListImpl currentPl PNil currentPl
else instance reversePListImplInductionStep ::
  ( ReversePListImpl (PCons name currentPl) restPl_t result
  ) => ReversePListImpl currentPl (PCons name restPl_t) result

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

-- | ParsePath

{- e.g.

a.b -> PCons "a" (PCons "b" PNil)
...b..c -> PCons "b" (PCons "c" PNil) -- ignore empty field names

-}

class ParsePath (path :: Symbol) (pl :: PList) | path -> pl

instance parsePathImpl ::
  ( ParsePathImpl "" PNil path reversed
  , ReversePList reversed pl
  ) => ParsePath path pl

{-
type ParseState =
  { currentName :: String
  , currentPl :: PList
  , pathLeftToParse :: String
  , result :: PList
  }

-}
-- NOTE result PList is reversed in order
class ParsePathImpl (currentName :: Symbol) (currentPl :: PList) (restPath :: Symbol) (result :: PList) | currentName currentPl restPath -> result

instance pathParseImplBaseCase1 ::
  ParsePathImpl "" pl "" pl
else instance pathParseImplBaseCase2 ::
  ParsePathImpl name pl "" (PCons name pl)
else instance pathParseImplBaseCase3 ::
  ParsePathImpl name pl "." (PCons name pl) -- TODO PathParse Error: end with "."
else instance parsePathImplDispatch ::
  ( Symbol.Cons h t restPath
  , ParsePathDispatch h name pl t result
  ) => ParsePathImpl name pl restPath result

class ParsePathDispatch (restPath_h :: Symbol) (currentName :: Symbol) (currentPl :: PList) (restPath_t :: Symbol) (result :: PList) | restPath_h currentName currentPl restPath_t -> result

instance parsePathDispatchBaseCase1 ::
  ( ParsePathImpl "" pl t result -- TODO ParsePath Error: empty field name
  ) => ParsePathDispatch "." "" pl t result
else instance parsePathDispatchBaseCase2 ::
  ( ParsePathImpl "" (PCons name pl) t result
  ) => ParsePathDispatch "." name pl t result
else instance parsePathDispatchInductionStep ::
  ( Symbol.Append name h name' -- name h -> name'
  , ParsePathImpl name' pl t result
  ) => ParsePathDispatch h name pl t result


-- | foreign functions

foreign import unsafePathPeek
  :: forall h row typ
   . Fn2
      (Array String)
      (STRecord h row)
      (ST h typ)

pathPeek
  :: forall h row path pl typ
   . ParsePath path pl
  => RowPListAccess row pl typ
  => PListToArray pl
  => SProxy path
  -> STRecord h row
  -> ST h typ
pathPeek _ = runFn2 unsafePathPeek (pListToArray (PLProxy :: PLProxy pl))


foreign import unsafePathPeekSTRecord
  :: forall h row childRow
   . Fn2
      (Array String)
      (STRecord h row)
      (ST h (STRecord h childRow))

pathPeekSTRecord
  :: forall h row path pl childRow
   . ParsePath path pl
  => RowPListAccessRecord row pl childRow
  => PListToArray pl
  => SProxy path
  -> STRecord h row
  -> ST h (STRecord h childRow)
pathPeekSTRecord _ = runFn2 unsafePathPeekSTRecord (pListToArray (PLProxy :: PLProxy pl))

foreign import unsafePeekLazyRef
  :: forall h row typ
   . Fn2
       String
       (STRecord h row)
       (ST h (Unit -> typ))

peekLazyRef
  :: forall h label row restRow typ
   . Row.Cons label typ restRow row
  => Symbol.IsSymbol label
  => SProxy label
  -> STRecord h row
  -> ST h (Unit -> typ)
peekLazyRef l = runFn2 unsafePeekLazyRef (Symbol.reflectSymbol l)

foreign import unsafePathPeekLazyRef
  :: forall h row childRow
   . Fn2
      (Array String)
      (STRecord h row)
      (ST h (Unit -> Record childRow))

pathPeekLazyRef
  :: forall h row path pl childRow
   . ParsePath path pl
  => RowPListAccessRecord row pl childRow
  => PListToArray pl
  => SProxy path
  -> STRecord h row
  -> ST h (Unit -> Record childRow)
pathPeekLazyRef _ = runFn2 unsafePathPeekLazyRef (pListToArray (PLProxy :: PLProxy pl))

foreign import unsafePathPoke
  :: forall h row typ
   . Fn3
      (Array String)
      typ
      (STRecord h row)
      (ST h Unit)

pathPoke
  :: forall h row path pl typ
   . ParsePath path pl
  => RowPListAccess row pl typ
  => PListToArray pl
  => SProxy path
  -> typ
  -> STRecord h row
  -> ST h Unit
pathPoke _ = runFn3 unsafePathPoke (pListToArray (PLProxy :: PLProxy pl))

foreign import unsafePathModify
  :: forall h row typ
   . Fn3
      (Array String)
      (typ -> typ)
      (STRecord h row)
      (ST h Unit)

pathModify
  :: forall h row path pl typ
   . ParsePath path pl
  => RowPListAccess row pl typ
  => PListToArray pl
  => SProxy path
  -> (typ -> typ)
  -> STRecord h row
  -> ST h Unit
pathModify _ = runFn3 unsafePathModify (pListToArray (PLProxy :: PLProxy pl))
