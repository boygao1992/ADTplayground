module Typelevel.Action where

import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Sum)
import Prelude

import Data.Maybe (Maybe)
import Prim.Row (class Cons) as Row
import Type.Row (RProxy(..))
import Type.Proxy (Proxy(..))

class GenericTensor sum (product :: # Type) | sum -> product

instance genericTensorSumToProduct ::
  ( GenericFromSumToProduct l r product
  ) => GenericTensor (Sum l r) product

class GenericFromSumToProduct l r (product :: # Type) | l r -> product

instance genericFromSumToProductBaseCase1 ::
  ( GenericArgumentToType l_arg l_type -- l_arg -> l_type
  , GenericArgumentToType r_arg r_type -- r_arg -> r_type
  , Row.Cons r_name (Maybe r_type) () row1 -- r_name r_type () -> row1
  , Row.Cons l_name (Maybe l_type) row1 product -- l_name l_type row1 -> product
  ) => GenericFromSumToProduct (Constructor l_name l_arg) (Constructor r_name r_arg) product
else instance genericFromSumToProductInductionStep ::
  ( GenericFromSumToProduct l r row1 -- l r -> row1
  , GenericArgumentToType arg typ
  , Row.Cons name (Maybe typ) row1 product -- name typ row1 -> product
  ) => GenericFromSumToProduct (Constructor name arg) (Sum l r) product

class GenericArgumentToType argument typ | argument -> typ

instance genericArgumentToTypeNoArguments ::
  GenericArgumentToType NoArguments Unit
else instance genericArgumentToTypeArgument ::
  GenericArgumentToType (Argument typ) typ
-- TODO else TypeError



-- | Test
sumToProduct :: forall i rep o. Generic i rep => GenericTensor rep o => Proxy i -> RProxy o
sumToProduct _ = RProxy :: RProxy o

type Entity = { id :: String, content :: String }

data Action
  = Create Entity
  | Read
  | Update
  | Delete { id :: String }
derive instance genericAction :: Generic Action _

actionObject :: RProxy
  ( "Create" :: Maybe
                  { id :: String
                  , content :: String
                  }
  , "Read" :: Maybe Unit
  , "Update" :: Maybe Unit
  , "Delete" :: Maybe
                  { id :: String
                  }
  )
actionObject = sumToProduct (Proxy :: Proxy Action)
