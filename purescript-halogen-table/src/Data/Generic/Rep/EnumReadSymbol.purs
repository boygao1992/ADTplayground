module Data.Generic.Rep.EnumReadSymbol where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Prim.TypeError (class Fail, Text)
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

enumReadSymbol :: forall a rep
  . Generic a rep
  => EnumReadSymbol rep
  => String
  -> Either String a
enumReadSymbol = map to <<< genericEnumReadSymbol

class EnumReadSymbol rep where
  genericEnumReadSymbol :: String -> Either String rep

instance enumReadSymbolBaseCase ::
  ( Symbol.IsSymbol name2
  ) => EnumReadSymbol (Constructor name2 NoArguments)
  where
    genericEnumReadSymbol name1 =
      if (name1 == name2)
      then
        Right $ Constructor NoArguments
      else
        Left $ name1 <> " doesn't match " <> name2

      where
        name2 = (Symbol.reflectSymbol (SProxy :: SProxy name2))
else
instance enumReadSymbolInductionStep ::
  ( EnumReadSymbol a
  , EnumReadSymbol b
  ) => EnumReadSymbol (Sum a b)
  where
    genericEnumReadSymbol name
        = Inl <$> genericEnumReadSymbol name
      <|> Inr <$> genericEnumReadSymbol name
else
instance enumReadSymbolNotEnum ::
  ( Fail
    (Text "not a valid Enum" )
  ) => EnumReadSymbol otherwise where
    genericEnumReadSymbol = genericEnumReadSymbol
