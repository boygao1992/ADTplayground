module Test.Main where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (logShow)
import Generic.EnumToArray (EnumVal, enumReadSymbol, enumToArray, enumToEnumValueArray)
import GraphQL.Type as G

-- | Test
data Action
  = Create
  | Read
  | Update
  | Delete
derive instance genericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow

example1 = (enumReadSymbol "Create") :: Either String Action

enumValueArrayExample = enumToEnumValueArray :: Array (G.EnumValue Action)

main :: Effect Unit
main = do
  logShow $ enumToArray :: Array (EnumVal Action)
