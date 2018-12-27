module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Type.Data.Boolean (BProxy, True, False)
import Type.Data.Symbol (SProxy(..))
import Utils (removeSpace, reverseSymbol)
import Digit (isDigitPred)
import Number (isNumberPred)

removeSpaceExample1 :: SProxy ""
removeSpaceExample1 = removeSpace $ SProxy :: SProxy ""

removeSpaceExample2 :: SProxy ""
removeSpaceExample2 = removeSpace $ SProxy :: SProxy " "

removeSpaceExample3 :: SProxy ""
removeSpaceExample3 = removeSpace $ SProxy :: SProxy "  "

removeSpaceExample4 :: SProxy "123"
removeSpaceExample4 = removeSpace $ SProxy :: SProxy " 1  2  3 "

reverseSymbolExample1 :: SProxy ""
reverseSymbolExample1 = reverseSymbol $ SProxy :: SProxy ""

reverseSymbolExample2 :: SProxy "321"
reverseSymbolExample2 = reverseSymbol $ SProxy :: SProxy "123"

reverseSymbolAfterRemoveSpaceExample :: SProxy "4321"
reverseSymbolAfterRemoveSpaceExample = reverseSymbol <<< removeSpace $ SProxy :: SProxy " 1 2 3 4 "

isDigitPredExample1 :: BProxy False
isDigitPredExample1 = isDigitPred $ SProxy :: SProxy ""

isDigitPredExample2 :: BProxy True
isDigitPredExample2 = isDigitPred $ SProxy :: SProxy "2"

isNumberPredExample1 :: BProxy False
isNumberPredExample1 = isNumberPred $ SProxy :: SProxy ""

isNumberPredExample2 :: BProxy True
isNumberPredExample2 = isNumberPred $ SProxy :: SProxy "123"

isNumberPredExample3 :: BProxy False
isNumberPredExample3 = isNumberPred $ SProxy :: SProxy "298723958-04"

main :: Effect Unit
main = do
  log "Hello sailor!"
