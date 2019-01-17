module Generic.EnumToArray where

import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import GraphQL.Type (EnumValue, enumValue)
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

newtype EnumVal rep = EnumVal
  { name :: String
  , constructor :: rep
  }

derive instance genericEnumVal :: Generic (EnumVal rep) _
derive instance newtypeEnumVal :: Newtype (EnumVal rep) _
derive instance functorEnumVal :: Functor EnumVal
instance showEnumVal :: Show rep => Show (EnumVal rep) where
  show = genericShow

enumToArray :: forall a rep. Generic a rep => GenericEnumToArray rep => Array (EnumVal a)
enumToArray = (map to) <$> genericEnumToArray

enumToEnumValueArray :: forall a rep. Generic a rep => GenericEnumToArray rep => Array (EnumValue a)
enumToEnumValueArray = map fromEnumVal enumToArray
  where
    fromEnumVal :: EnumVal a -> EnumValue a
    fromEnumVal (EnumVal {name, constructor}) = enumValue name Nothing constructor

-- assumption: IsEnum rep =>
class GenericEnumToArray rep where
  genericEnumToArray :: Array (EnumVal rep)

instance genericEnumToArrayBaseCase ::
  ( Symbol.IsSymbol name
  ) => GenericEnumToArray (Constructor name NoArguments)
  where
    genericEnumToArray =
      [ EnumVal
          { name : Symbol.reflectSymbol (SProxy :: SProxy name)
          , constructor : Constructor NoArguments
          }
      ]
else instance genericEnumToArrayInductionStep ::
  ( GenericEnumToArray l
  , GenericEnumToArray r
  ) => GenericEnumToArray (Sum l r)
  where
    genericEnumToArray
       = ((map Inl) <$> (genericEnumToArray :: Array (EnumVal l)))
      <> ((map Inr) <$> (genericEnumToArray :: Array (EnumVal r)))


