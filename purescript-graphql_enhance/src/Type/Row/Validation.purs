module Type.Row.Validation where

import Prim.RowList (kind RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Type.Data.Boolean as Bool
import Type.Data.Symbol (SProxy(..))
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import Type.Row.Utils as Row
import Type.Utils as Type

foreign import kind Result
foreign import data Failure :: Symbol -> Result
foreign import data Success :: Result

data RSProxy (result :: Result) = RSProxy

data Required a
data Optional a
data Repelled -- NOTE for Validate only, not for ValidateExclusive
data RequiredField -- NOTE no type check
data OptionalField -- NOTE no type check

-- | AppendResult
class Append (r1 :: Result) (r2 :: Result) (r :: Result) | r1 r2 -> r

instance appendSS :: Append Success Success Success
instance appendSF :: Append Success (Failure err) (Failure err)
instance appendFS :: Append (Failure err) Success (Failure err)
instance appendFF ::
  ( Symbol.Append err1 " |> " err0
  , Symbol.Append err0 err2 err
  ) => Append (Failure err1) (Failure err2) (Failure err)

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
  ) => ValidatePatternDispatch (Row.FetchSuccess typ1 restRow) name (Optional typ2) o
else instance validatePatternFetchSuccessRequired ::
  ( Type.IsEqualPred typ1 typ2 isEqual
  , ValidatePatternFetchSuccessDispatch isEqual name o
  ) => ValidatePatternDispatch (Row.FetchSuccess typ1 restRow) name (Required typ2) o
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

-- | ValidateExclusive
class ValidateExclusive (schema :: # Type) (i :: # Type)

instance validateExclusiveToRowList ::
  ( RowList.RowToList schema schemaRl
  , ValidateExclusiveRowList schemaRl i
  ) => ValidateExclusive schema i

class ValidateExclusiveRowList (schemaRl :: RowList) (i :: # Type)

instance validateExlusiveRowListNil ::
  ( Row.IsEmptyPred i isEmpty
  , ValidateExclusiveExhausted isEmpty i
  ) => ValidateExclusiveRowList RowList.Nil i
else instance validateExlusiveRowListConsOptional ::
  ( Row.FetchField name i fetchResult
  , ValidateExclusiveRowListDispatch fetchResult name (Optional typ) restSchemaRl i
  ) => ValidateExclusiveRowList (RowList.Cons name (Optional typ) restSchemaRl) i
else instance validateExlusiveRowListConsRequired ::
  ( Row.FetchField name i fetchResult
  , ValidateExclusiveRowListDispatch fetchResult name (Required typ) restSchemaRl i
  ) => ValidateExclusiveRowList (RowList.Cons name (Required typ) restSchemaRl) i
else instance validateExlusiveRowListConsRequiredField ::
  ( Row.FetchField name i fetchResult
  , ValidateExclusiveRowListDispatch fetchResult name RequiredField restSchemaRl i
  ) => ValidateExclusiveRowList (RowList.Cons name RequiredField restSchemaRl) i
else instance validateExlusiveRowListConsOptionalField ::
  ( Row.FetchField name i fetchResult
  , ValidateExclusiveRowListDispatch fetchResult name OptionalField restSchemaRl i
  ) => ValidateExclusiveRowList (RowList.Cons name OptionalField restSchemaRl) i
else instance validateExlusiveRowListInvalidPattern ::
  Fail
  ( Above
    ( Beside
      ( Text "Invalid pattern for field `")
      ( Beside (Text name) (Text "`:"))
    )
    ( Quote pattern)
  )
  => ValidateExclusiveRowList (RowList.Cons name pattern restSchemaRl) i

class ValidateExclusiveExhausted (isEmpty :: Bool.Boolean) (row :: # Type)

instance validateExclusiveExhaustedYes ::
  ValidateExclusiveExhausted Bool.True row
else instance validateExclusiveExhaustedNo ::
  Fail
  (Above
    (Text "Redundant fields: ")
    (Quote (Record row))
  )
  => ValidateExclusiveExhausted Bool.False row

class ValidateExclusiveRowListDispatch (fetchResult :: Row.FetchResult) (name :: Symbol) pattern (restSchemaRl :: RowList) (i :: # Type)

instance validateExclusiveRowListFetchFailureOptional ::
  ( ValidateExclusiveRowList restSchemaRl i
  ) => ValidateExclusiveRowListDispatch Row.FetchFailure name (Optional typ) restSchemaRl i
else instance validateExclusiveRowListFetchFailureOptionalField ::
  ( ValidateExclusiveRowList restSchemaRl i
  ) => ValidateExclusiveRowListDispatch Row.FetchFailure name OptionalField restSchemaRl i
else instance validateExclusiveRowListFetchFailureRequired ::
  Fail
  (Beside
   (Text "Required field `") (Beside (Text name) (Text "` is not provided."))
  )
  => ValidateExclusiveRowListDispatch Row.FetchFailure name (Required typ) restSchemaRl i
else instance validateExclusiveRowListFetchFailureRequiredField ::
  Fail
  (Beside
   (Text "Required field `") (Beside (Text name) (Text "` is not provided."))
  )
  => ValidateExclusiveRowListDispatch Row.FetchFailure name RequiredField restSchemaRl i
else instance validateExclusiveRowListFetchSuccessOptionalField ::
  ( ValidateExclusiveRowList restSchemaRl restI
  ) => ValidateExclusiveRowListDispatch (Row.FetchSuccess typ1 restI) name OptionalField restSchemaRl i
else instance validateExclusiveRowListFetchSuccessRequiredField ::
  ( ValidateExclusiveRowList restSchemaRl restI
  ) => ValidateExclusiveRowListDispatch (Row.FetchSuccess typ1 restI) name RequiredField restSchemaRl i
else instance validateExclusiveRowListFetchSuccessOptional ::
  ( Type.IsEqualPred typ1 typ2 isEqual
  , ValidateExclusiveRowListFetchSuccessOptional isEqual name typ1 typ2 restSchemaRl restI
  ) => ValidateExclusiveRowListDispatch (Row.FetchSuccess typ1 restI) name (Optional typ2) restSchemaRl i
else instance validateExclusiveRowListFetchSuccessRequired ::
  ( Type.IsEqualPred typ1 typ2 isEqual
  , ValidateExclusiveRowListFetchSuccessRequired isEqual name typ1 typ2 restSchemaRl restI
  ) => ValidateExclusiveRowListDispatch (Row.FetchSuccess typ1 restI) name (Required typ2) restSchemaRl i

class ValidateExclusiveRowListFetchSuccessOptional (isEqual :: Bool.Boolean) (name :: Symbol) typ1 typ2 (restSchemaRl :: RowList) (restI :: # Type)

instance validateExclusiveRowListFetchSuccessOptionalIsEqual ::
  ( ValidateExclusiveRowList restSchemaRl restI
  ) => ValidateExclusiveRowListFetchSuccessOptional Bool.True name typ typ restSchemaRl restI
else instance validateExclusiveRowListFetchSuccessOptionalNotEqual ::
  Fail
  ( Above
    ( Beside
      ( Text "Type mismatch occurred on Optional field `")
      ( Beside (Text name) (Text "`.")
      )
    )
    ( Above
      ( Beside (Text "Expected: ") (Quote typ1))
      ( Beside (Text "Actual:   ") (Quote typ2))
    )
  )
  => ValidateExclusiveRowListFetchSuccessOptional Bool.False name typ1 typ2 restSchemaRl restI

class ValidateExclusiveRowListFetchSuccessRequired (isEqual :: Bool.Boolean) (name :: Symbol) typ1 typ2 (restSchemaRl :: RowList) (restI :: # Type)

instance validateExclusiveRowListFetchSuccessRequiredIsEqual ::
  ( ValidateExclusiveRowList restSchemaRl restI
  ) => ValidateExclusiveRowListFetchSuccessRequired Bool.True name typ typ restSchemaRl restI
else instance validateExclusiveRowListFetchSuccessRequiredNotEqual ::
  Fail
  ( Above
    ( Beside
      ( Text "Type mismatch occurred on Required field `")
      ( Beside (Text name) (Text "`.")
      )
    )
    ( Above
      ( Beside (Text "Expected: \t") (Quote typ1))
      ( Beside (Text "Actual: \t") (Quote typ2))
    )
  )
  => ValidateExclusiveRowListFetchSuccessRequired Bool.False name typ1 typ2 restSchemaRl restI

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


-- validateExclusive
--   :: forall patterns i
--    . ValidateExclusive patterns i
--   => RProxy patterns
--   -> RProxy i
--   -> Unit
-- validateExclusive _ _ = unit

-- validateExclusiveExample1 =
--   validateExclusive
--   ( RProxy :: RProxy  ( name :: Required String
--                       , description :: Optional String
--                       , fields :: Required {}
--                       )
--   )
--   ( RProxy :: RProxy  ( name :: String
--                       , description :: String
--                       , fields :: {}
--                       , rest :: String
--                       )
--   )
--
-- A custom type error occurred while solving type class constraints:
--     Redundant fields:
--     { rest :: String
--     }
