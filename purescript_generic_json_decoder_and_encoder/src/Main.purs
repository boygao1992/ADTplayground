module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (log)
-- import Foreign.Class (class Encode, class Decode, encode, decode)
import Foreign.Generic (defaultOptions, genericDecodeJSON, genericEncodeJSON)
import Data.Either (either)

newtype User = User
  { name :: String
  , age :: Int
  }
derive instance genericUser :: Generic User _
instance showUser :: Show User where
  show = genericShow

jsonOptions = defaultOptions { unwrapSingleConstructors = true }

main :: Effect Unit
main = do
  log $ genericEncodeJSON jsonOptions (User { name : "wenbo", age : 26 })
  log $ either (const "error") show $ runExcept (genericDecodeJSON jsonOptions "{\"name\":\"wenbo\",\"age\":26}" :: _ User)
