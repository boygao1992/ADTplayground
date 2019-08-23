module Test.Table where

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

newtype Person = Person
  { name :: String
  , age :: Int
  , pet :: Maybe String
  , cash :: Number
  }
derive instance newtypePerson :: Newtype Person _

