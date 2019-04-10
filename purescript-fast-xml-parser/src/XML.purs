module XML where

import Data.Function.Uncurried (Fn1, runFn1)
import Foreign (Foreign)

foreign import _xml2json :: Fn1 String Foreign
foreign import _json2xml :: Fn1 Foreign String

xml2json :: String -> Foreign
xml2json xml = runFn1 _xml2json xml

json2xml :: Foreign -> String
json2xml json = runFn1 _json2xml json
