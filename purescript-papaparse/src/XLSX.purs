module XLSX where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn3)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..), snd)
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
toCSVs :: String -> Effect (Either String (Map String String))
toCSVs content =
  map fromFoldable <$> runEffectFn1 (runFn3 _toCSV Left Right (mkFn2 Tuple)) content

toCSV :: String -> Effect (Either String String)
toCSV content =
  (bind <@> note "" <<< map snd <<< Array.head)
    <$> runEffectFn1 (runFn3 _toCSV Left Right (mkFn2 Tuple)) content
