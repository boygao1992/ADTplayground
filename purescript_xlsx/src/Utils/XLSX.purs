module Utils.XLSX where

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import _toCSV ::
  Fn2
    (String -> Either String String)
    (String -> Either String String)
    (EffectFn2
       String
       String
       (Either String String)
    )

-- NOTE content has to be encoded in Base64
-- TODO try catch
toCSV :: String -> String -> Effect (Either String String)
toCSV content tableName = runEffectFn2 (runFn2 _toCSV Left Right ) content tableName
