module XML where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.List.NonEmpty as NEL
import Foreign (Foreign, renderForeignError)
import Simple.JSON (class ReadForeign, read)

-- | Utils

safeRead
  :: forall a
   . ReadForeign a
  => Foreign
  -> Either String a
safeRead = lmap (show <<< NEL.toList <<< map renderForeignError) <<< read

-- | Foreign

foreign import _xml2json :: Fn1 String Foreign
foreign import _json2xml :: Fn1 Foreign String

-- | API

xml2json
  :: forall a
   . ReadForeign a
  => String
  -> Either String a
xml2json xml = safeRead $ runFn1 _xml2json xml

json2xml :: Foreign -> String
json2xml json = runFn1 _json2xml json

