module Utils.XLSX where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn3)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import _toCSV ::
  Fn3
    (String -> Either String String)
    (String -> Either String String)
    (Fn2 String String (Tuple String String))
    (EffectFn1
       String
       (Either String (Array (Tuple String String)))
    )

-- NOTE content has to be encoded in Base64
toCSV :: String -> Effect (Either String (Map String String))
toCSV content =
  map fromFoldable <$> runEffectFn1 (runFn3 _toCSV Left Right (mkFn2 Tuple)) content
