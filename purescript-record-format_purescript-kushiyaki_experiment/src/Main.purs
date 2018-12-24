module Main where

import Prelude

-- import Data.Either (Either)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log, logShow)
import Kushiyaki (parseURL)
import Record.Format (format)
import Type.Data.Symbol (SProxy(..))
import RuntimeParser (parse) as RP
import Record.Format.RuntimeParser (parse) as FRRP

-- | inferred Type of formatter, given the type-level template literal containing two `FormatVar`s, `name` and `number`
-- formatter
--   :: forall a b r
--   .  FormatVar a
--   => FormatVar b
--   => { name :: a
--      , number :: b
--      | r
--      }
--   -> String
-- formatter =
--   format
--     (SProxy :: SProxy "Hi {name}! Your favorite number is {number}.")

type User =
  { name :: String
  , age :: Int
  }

url :: String
url = "/user/create/Bill/12"

endpoint = SProxy :: SProxy "/user/create/{name:String}/{age:Int}"

main :: Effect Unit
main = do
  log formatted

  for_ (parseURL endpoint url) \user -> do
    logShow user

  -- logShow $ RP.parse "/user/create/${name:String}/${age:Int}"
  logShow $ FRRP.parse "Hi {name}! Your favorite number is {number}."

  where
    formatted :: String
    formatted =
      format
        (SProxy :: SProxy "Hi {name}! Your favorite number is {number}")
        { name : "Bill", number : 16 }
