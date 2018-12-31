module Typelevel.GenericTensor where

import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Sum)
import Data.Maybe (Maybe)
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import Typelevel.Utils (class SwitchCaseFirstChar)

class GenericTensor sum record | sum -> record

instance genericTensorSumToRow ::
  ( GenericFromSumToRow l r row
  ) => GenericTensor (Sum l r) (Record row)

class GenericFromSumToRow l r (row :: # Type) | l r -> row

instance genericFromSumToRowBaseCase1 ::
  ( GenericArgumentAndType l_arg l_type -- l_arg -> l_type
  , GenericArgumentAndType r_arg r_type -- r_arg -> r_type
  , SwitchCaseFirstChar l_name l_name_lower
  , SwitchCaseFirstChar r_name r_name_lower
  , Row.Cons r_name_lower (Maybe r_type) () row1 -- r_name_lower r_type () -> row1
  , Row.Cons l_name_lower (Maybe l_type) row1 row -- l_name_lower l_type row1 -> row
  ) => GenericFromSumToRow (Constructor l_name l_arg) (Constructor r_name r_arg) row
else instance genericFromSumToRowInductionStep ::
  ( GenericFromSumToRow l r row1 -- l r -> row1
  , GenericArgumentAndType arg typ
  , SwitchCaseFirstChar name name_lower
  , Row.Cons name_lower (Maybe typ) row1 row -- name typ row1 -> row
  ) => GenericFromSumToRow (Constructor name arg) (Sum l r) row


class GenericFromRowToSum (row :: # Type) sum | row -> sum

instance genericFromRowToSumBaseCase ::
  ( Row.Cons name typ () row -- name typ () <- row
  , SwitchCaseFirstChar name_upper name -- name_upper <- name
  , GenericArgumentAndType arg typ -- arg <- typ
  ) => GenericFromRowToSum row (Constructor name_upper arg)
else instance genericFromRowToSumInductionStep ::
  ( Row.Cons name typ restRow row -- name typ restRow <- row
  , SwitchCaseFirstChar name_upper name -- name_upper <- name
  , GenericArgumentAndType arg typ -- arg <- typ
  , GenericFromRowToSum restRow restSum
  ) => GenericFromRowToSum row (Sum (Constructor name_upper arg) restSum)



class GenericArgumentAndType argument typ | argument -> typ, typ -> argument

instance genericArgumentAndTypeNoArguments ::
  GenericArgumentAndType NoArguments Unit
else instance genericArgumentAndTypeArgument ::
  GenericArgumentAndType (Argument typ) typ
-- TODO else TypeError

-- | Generic Tensor Match
-- | match :: Generic o rep => GenericTensor rep i => Record i -> Either String o
-- example 1, success
-- | { create : Nothing, read : Just unit, update : Nothing, delete : Nothing }
-- |   -> Right (Read unit)
-- example 2, failure
-- | { create : Nothing, read : Just unit, update : Just unit, delete : Nothing }
-- |   -> Left "more than one active command"
-- example 3, failure
-- | { create : Nothing, read : Nothing, update : Nothing, delete : Nothing }
-- |   -> Left "no active command"

-- | Test
sumToRow :: forall i l r o. Generic i (Sum l r) => GenericFromSumToRow l r o => Proxy i -> RProxy o
sumToRow _ = RProxy :: RProxy o

type Entity = { id :: String, content :: String }

data Action
  = Create Entity
  | Read
  | Update
  | Delete { id :: String }
derive instance genericAction :: Generic Action _

actionObject :: RProxy
  ( create :: Maybe
                { id :: String
                , content :: String
                }
  , read :: Maybe Unit
  , update :: Maybe Unit
  , delete :: Maybe
                { id :: String
                }
  )
actionObject = sumToRow (Proxy :: Proxy Action)
