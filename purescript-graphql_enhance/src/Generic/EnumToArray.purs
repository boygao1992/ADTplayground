module EnumToArray where

import Data.Generic.Rep
import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import GraphQL.Type (EnumValue, enumValue)
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Proxy (Proxy)

-- assumption: IsEnum rep =>
-- class EnumToArray a rep | a -> rep where
--   enumToArray :: Generic a rep => Array (EnumValue a)

-- instance enumToArrayBaseCase ::
--   ( Symbol.IsSymbol name
--   ) => EnumToArray a (Constructor name NoArguments)
--   where
--     enumToArray = [ enumValue
--                       (Symbol.reflectSymbol (SProxy :: SProxy name))
--                       Nothing
--                       (to (Constructor NoArguments))
--                   ]
-- else instance enumToArrayInductionStep ::
--   (
--   ) => EnumToArray a ()

enumReadSymbol :: forall a rep
   . Generic a rep
  => EnumReadSymbol rep
  => String
  -> Either String a
enumReadSymbol = map to <<< enumReadSymbolImpl

class EnumReadSymbol rep where
  enumReadSymbolImpl :: String -> Either String rep

instance enumReadSymbolBaseCase ::
  ( Symbol.IsSymbol name2
  ) => EnumReadSymbol (Constructor name2 NoArguments)
  where
    enumReadSymbolImpl name1 =
      if (name1 == name2)
      then
        Right $ Constructor NoArguments
      else
        Left $ name1 <> " doesn't match " <> name2

      where
        name2 = (Symbol.reflectSymbol (SProxy :: SProxy name2))
else instance enumReadSymbolInductionStep ::
  ( EnumReadSymbol a
  , EnumReadSymbol b
  ) => EnumReadSymbol (Sum a b)
  where
    enumReadSymbolImpl name = Inl <$> enumReadSymbolImpl name
                          <|> Inr <$> enumReadSymbolImpl name

-- | Test
data Action
  = Create
  | Read
  | Update
  | Delete
derive instance genericAction :: Generic Action _

example1 = (enumReadSymbol "Create") :: Either String Action
