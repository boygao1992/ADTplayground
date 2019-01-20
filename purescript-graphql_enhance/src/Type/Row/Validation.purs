module Type.Row.Validation where

import Prim.Symbol as Symbol
import Type.Data.Boolean as Bool
import Type.Data.Symbol (SProxy(..))
import Type.Row (RProxy(..))
import Type.Row.Utils as Row
import Type.Utils as Type
import Type.Proxy (Proxy(..))
import Prim.RowList (kind RowList)
import Prim.RowList as RowList

foreign import kind Result
foreign import data Failure :: Symbol -> Result
foreign import data Success :: Result

data RSProxy (result :: Result) = RSProxy

data Required a
data Optional a
data Repelled
{- TODO
foreign import kind Pattern
foreign import data _Required :: Type -> Pattern
foreign import data _Optional :: Type -> Pattern

data PProxy (pattern :: Pattern)

type Required a = PProxy (_Required a)
type Optional a = PProxy (_Optional a)
-}

-- | AppendResult
class Append (r1 :: Result) (r2 :: Result) (r :: Result) | r1 r2 -> r

instance appendSS :: Append Success Success Success
instance appendSF :: Append Success (Failure err) (Failure err)
instance appendFS :: Append (Failure err) Success (Failure err)
instance appendFF ::
  ( Symbol.Append err1 " |> " err0
  , Symbol.Append err0 err2 err
  ) => Append (Failure err1) (Failure err2) (Failure err)

-- | ValidatePattern
class ValidatePattern (name :: Symbol) (pattern :: Type) (i :: # Type) (o :: Result) | name pattern i -> o

instance validatePatternFetchField ::
  ( Row.FetchField name i fetchResult
  , ValidatePatternDispatch fetchResult name pattern o
  ) => ValidatePattern name pattern i o

class ValidatePatternDispatch (fetchResult :: Row.FetchResult) (name :: Symbol) (pattern :: Type) (o :: Result) | fetchResult name pattern -> o

instance validatePatternFetchFailureOptional ::
  ValidatePatternDispatch Row.FetchFailure name (Optional typ) Success
else instance validatePatternFetchFailureRequired ::
  ( Symbol.Append "Required field `" name err0
  , Symbol.Append err0 "` is not provided." err
  ) => ValidatePatternDispatch Row.FetchFailure name (Required typ) (Failure err)
else instance validatePatternFetchFailureRepelled ::
  ValidatePatternDispatch Row.FetchFailure name Repelled Success
else instance validatePatternFetchSuccessOptional ::
  ( Type.IsEqualPred typ1 typ2 isEqual
  , ValidatePatternFetchSuccessDispatch isEqual name o
  ) => ValidatePatternDispatch (Row.FetchSuccess typ1) name (Optional typ2) o
else instance validatePatternFetchSuccessRequired ::
  ( Type.IsEqualPred typ1 typ2 isEqual
  , ValidatePatternFetchSuccessDispatch isEqual name o
  ) => ValidatePatternDispatch (Row.FetchSuccess typ1) name (Required typ2) o
else instance validatePatternFetchSuccessRepelled ::
  ( Symbol.Append "Repelled field `" name err0
  , Symbol.Append err0 "` is provided." err
  ) => ValidatePatternDispatch Row.FetchFailure name Repelled (Failure err)

class ValidatePatternFetchSuccessDispatch (isEqual :: Bool.Boolean) (name :: Symbol) (o :: Result) | isEqual name -> o

instance validatePatternFetchSuccessIsEqual ::
  ValidatePatternFetchSuccessDispatch Bool.True name Success
else instance validatePatternFetchSuccessNotEqual ::
  ( Symbol.Append "Field `" name err0
  , Symbol.Append err0 "` has a type mismatch." err
  ) => ValidatePatternFetchSuccessDispatch Bool.False name (Failure err)

-- | Validate
class Validate (schema :: # Type) (i :: # Type) (o :: Result) | schema i -> o

instance validateToRowList ::
  ( RowList.RowToList schema schemaRl
  , ValidateRowList schemaRl i o
  ) => Validate schema i o

class ValidateRowList (schemaRl :: RowList) (i :: # Type) (o :: Result) | schemaRl i -> o

instance validateRowListBaseCase ::
  ValidateRowList RowList.Nil i Success
else instance validateRowListInductionStep ::
  ( ValidateRowList restRl i restO
  , ValidatePattern name pattern i o'
  , Append o' restO o
  ) => ValidateRowList (RowList.Cons name pattern restRl) i o

-- Test

validatePattern
  :: forall name pattern i o
   . ValidatePattern name pattern i o
  => SProxy name
  -> Proxy pattern
  -> RProxy i
  -> RSProxy o
validatePattern _ _ _ = RSProxy :: RSProxy o

validatePatternExample1 :: RSProxy Success
validatePatternExample1 = validatePattern
                          (SProxy :: SProxy "id")
                          (Proxy :: Proxy (Required String))
                          (RProxy :: RProxy ( id :: String ))

validatePatternExample2 :: RSProxy (Failure "Required field `id` is not provided.")
validatePatternExample2 = validatePattern
                          (SProxy :: SProxy "id")
                          (Proxy :: Proxy (Required String))
                          (RProxy :: RProxy ( name :: String ))

validatePatternExample3 :: RSProxy (Failure "Field `id` has a type mismatch.")
validatePatternExample3 = validatePattern
                          (SProxy :: SProxy "id")
                          (Proxy :: Proxy (Required Int))
                          (RProxy :: RProxy ( id :: String ))


validatePatternExample4 :: RSProxy Success
validatePatternExample4 = validatePattern
                          (SProxy :: SProxy "id")
                          (Proxy :: Proxy (Optional String))
                          (RProxy :: RProxy ( id :: String ))

validatePatternExample5 :: RSProxy Success
validatePatternExample5 = validatePattern
                          (SProxy :: SProxy "id")
                          (Proxy :: Proxy (Optional String))
                          (RProxy :: RProxy ( name :: String ))

validatePatternExample6 :: RSProxy (Failure "Field `id` has a type mismatch.")
validatePatternExample6 = validatePattern
                          (SProxy :: SProxy "id")
                          (Proxy :: Proxy (Optional Int))
                          (RProxy :: RProxy ( id :: String ))

validate
  :: forall patterns i o
   . Validate patterns i o
  => RProxy patterns
  -> RProxy i
  -> RSProxy o
validate _ _ = RSProxy :: RSProxy o

validateExample1 :: RSProxy Success
validateExample1 = validate
                   ( RProxy :: RProxy ( name :: Required String
                                      , description :: Optional String
                                      , fields :: Required {}
                                      )
                   )
                   ( RProxy :: RProxy ( name :: String
                                      , fields :: {}
                                      )
                   )

validateExample2 :: RSProxy (Failure "Required field `fields` is not provided. |> Field `name` has a type mismatch.")
validateExample2 = validate
                  ( RProxy :: RProxy  ( name :: Required String
                                      , description :: Optional String
                                      , fields :: Required {}
                                      )
                  )
                  ( RProxy :: RProxy  ( name :: Int
                                      )
                  )
