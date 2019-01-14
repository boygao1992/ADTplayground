module RecordRef where

import Prelude

import Data.Array ((:))
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)
import Effect (Effect)
import Effect.Ref (Ref)
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
modify f = modify' reducer
  where
    reducer :: Record row -> { state :: Record row, value :: Record row }
    reducer rec = let rec' = f rec in { state : rec', value : rec' }

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
  :: forall row path pl typ
   . ParsePath path pl
  => RowPListAccess row pl typ
  => PListToArray pl
  => SProxy path
  -> RecordRef row
  -> Effect typ
pathRead _ = runFn2 _pathRead (pListToArray (PLProxy :: PLProxy pl))

-- HACK dangerous, entangle the input RecordRef and the output RecordRef
-- TODO restrict path to leave the leaf nodes untouched
-- TODO add extra type parameters to denote such an entanglement
foreign import _pathReadRef
  :: forall row typ
   . Fn2
      (Array String)
      (RecordRef row)
      (Effect (RecordRef typ))

pathReadRef
  :: forall row path pl row'
   . ParsePath path pl
  => RowPListAccess row pl (Record row')
  => PListToArray pl
  => SProxy path
  -> RecordRef row
  -> Effect (RecordRef row')
pathReadRef _ = runFn2 _pathReadRef (pListToArray (PLProxy :: PLProxy pl))


foreign import _pathModify'
  :: forall row typ b
   . Fn3
       (Array String)
       (typ -> { state :: typ, value :: b })
       (RecordRef row)
       (Effect b)

pathModify'
  :: forall row path pl typ b
   . ParsePath path pl
  => RowPListAccess row pl typ
  => PListToArray pl
  => SProxy path
  -> (typ -> { state :: typ, value :: b })
  -> RecordRef row
  -> Effect b
pathModify' _ = runFn3 _pathModify' (pListToArray (PLProxy :: PLProxy pl))

pathModify
  :: forall row path pl typ
   . ParsePath path pl
  => RowPListAccess row pl typ
  => PListToArray pl
  => SProxy path
  -> (typ -> typ)
  -> RecordRef row
  -> Effect typ
pathModify path f = pathModify' path reducer
  where
    reducer :: typ -> { state :: typ, value :: typ }
    reducer rec = let rec' = f rec in { state : rec', value : rec' }

pathModify_
  :: forall row path pl typ
   . ParsePath path pl
  => RowPListAccess row pl typ
  => PListToArray pl
  => SProxy path
  -> (typ -> typ)
  -> RecordRef row
  -> Effect Unit
pathModify_ path f ref = void $ pathModify path f ref

foreign import _pathWrite
  :: forall row typ
   . Fn3
       (Array String)
       typ
       (RecordRef row)
       (Effect Unit)

pathWrite
  :: forall row path pl typ
   . ParsePath path pl
  => RowPListAccess row pl typ
  => PListToArray pl
  => SProxy path
  -> typ
  -> RecordRef row
  -> Effect Unit
pathWrite _ = runFn3 _pathWrite (pListToArray (PLProxy :: PLProxy pl))

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

-- | RowPListAccess
class RowPListAccess (row :: # Type) (pl :: PList) typ | row pl -> typ

instance rowPListAccesBaseCase1 ::
  RowPListAccess row PNil (Record row)
else instance rowPListAccessBaseCase2 ::
  ( Row.Cons name typ restRow row
  ) => RowPListAccess row (PCons name PNil) typ
else instance rowPListAccessInductionStep ::
  ( Row.Cons name (Record childRow) restRow row
  , RowPListAccess childRow restPl typ
  ) => RowPListAccess row (PCons name restPl) typ
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


-- | Test
data PLProxy (pl :: PList) = PLProxy

-- class ValidPathPred
validPathPred :: forall row pl b. ValidPathPred row pl b => RProxy row -> PLProxy pl -> Bool.BProxy b
validPathPred _ _ = Bool.BProxy :: Bool.BProxy b

validPathPredExample :: Bool.BProxy Bool.True
validPathPredExample = validPathPred
                       (RProxy :: RProxy ( a :: { b :: Int }, c :: String ))
                       (PLProxy :: PLProxy (PCons "a" (PCons "b" PNil)))

-- class RowPListAccess
rowPListAccess :: forall row pl typ. RowPListAccess row pl typ => RProxy row -> PLProxy pl -> Proxy typ
rowPListAccess _ _ = Proxy :: Proxy typ

rowPListAccessExample :: Proxy Int
rowPListAccessExample = rowPListAccess
                       (RProxy :: RProxy ( a :: { b :: Int }, c :: String ))
                       (PLProxy :: PLProxy (PCons "a" (PCons "b" PNil)))

parsePath :: forall path pl. ParsePath path pl => SProxy path -> PLProxy pl
parsePath _ = PLProxy :: PLProxy pl

parsePathExample1 :: PLProxy (PCons "a" (PCons "b" PNil))
parsePathExample1 = parsePath (SProxy :: SProxy "a.b")

parsePathExample2 :: PLProxy (PCons "a" PNil)
parsePathExample2 = parsePath (SProxy :: SProxy "a.")

parsePathExample3 :: PLProxy (PCons "" PNil)
parsePathExample3 = parsePath (SProxy :: SProxy "...")
