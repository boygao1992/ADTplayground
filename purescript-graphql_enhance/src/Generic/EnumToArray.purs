module EnumToArray where

import Data.Generic.Rep
import Prelude

import Data.Maybe (Maybe(..))
import GraphQL.Type (EnumValue, enumValue)
import Type.Proxy (Proxy)
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

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
