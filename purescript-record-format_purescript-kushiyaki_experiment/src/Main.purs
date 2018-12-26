module Main where

import Prelude

-- import Data.Either (Either)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log, logShow)
import Kushiyaki.Annotated (parseUrl)
-- import Record.Format (format)
import Type.Data.Symbol (SProxy(..))
-- import RuntimeParser (parse) as RP
import Record.Format.RuntimeParser (parse) as FRRP
import Format (format)
import Kushiyaki.RuntimeParser as KR

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

  for_ (parseUrl endpoint url) \user -> do
    logShow $ user :: User

  -- logShow $ RP.parse "/user/create/${name:String}/${age:Int}"
  logShow $ FRRP.parse "Hi {name}! Your favorite number is {number}."

  logShow $ FRRP.parse KR.sampleTemplate

  logShow $ KR.parseTypedParam "name:String"
  logShow $ KR.parseTypedParam "number:Int"
  logShow $ KR.parseTypedParam "hello:world" -- Error
  logShow $ KR.parseTypedParam "name" -- default type, String

  logShow $ KR.parseUrl "/user/create/{name:String}/{age:Int}"

  where
    formatted :: String
    formatted =
      format
        (SProxy :: SProxy "Hi {name}! Your favorite number is {number}")
        { number : 12, name : "Bill" }
