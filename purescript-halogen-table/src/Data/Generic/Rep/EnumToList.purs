module Data.Generic.Rep.EnumToList where

import Data.Generic.Rep (class Generic, Constructor, NoArguments, Sum)
import Data.List (List(Nil), (:))
import Prim.TypeError (class Fail, Text)
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy(..))

enumToList
  :: forall a rep
  . Generic a rep
  => GenericEnumToList rep
  => Proxy a
  -> List String
enumToList _ = genericEnumToList (Proxy :: Proxy rep)

class GenericEnumToList rep where
  genericEnumToList :: Proxy rep -> List String

instance genericEnumToListInductionStep ::
  ( Symbol.IsSymbol label
  , GenericEnumToList r
  ) => GenericEnumToList (Sum (Constructor label NoArguments) r)
  where
    genericEnumToList _
      = Symbol.reflectSymbol (SProxy :: SProxy label)
      : genericEnumToList (Proxy :: Proxy r)
else
instance genericEnumToListBaseCase
  :: ( Symbol.IsSymbol label
    )
  => GenericEnumToList (Constructor label NoArguments)
  where
    genericEnumToList _
      = Symbol.reflectSymbol (SProxy :: SProxy label)
      : Nil
else
instance genericEnumToListError
  :: ( Fail
      ( Text "not a valid Enum"
      )
    )
  => GenericEnumToList otherwise
  where
    genericEnumToList = genericEnumToList


