module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Type.Data.Boolean (BProxy, True, False)
import Type.Data.Symbol (SProxy(..))
import Utils (removeSpace, reverseSymbol)
import Digit (isDigitPred)
import Number (isNumberPred, add) as Number

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
isNumberPredExample1 = Number.isNumberPred $ SProxy :: SProxy ""

isNumberPredExample2 :: BProxy True
isNumberPredExample2 = Number.isNumberPred $ SProxy :: SProxy "123"

isNumberPredExample3 :: BProxy False
isNumberPredExample3 = Number.isNumberPred $ SProxy :: SProxy "298723958-04"

addNumberExample1 :: SProxy "10000"
addNumberExample1 = Number.add (SProxy :: SProxy "1") (SProxy :: SProxy "9999")

addNumberExample2 :: SProxy "1999998"
addNumberExample2 = Number.add (SProxy :: SProxy "999999") (SProxy :: SProxy "999999")

addNumberExample3 :: SProxy "968623218844"
addNumberExample3 = Number.add (SProxy :: SProxy "8942758984") (SProxy :: SProxy "959680459860")

main :: Effect Unit
main = do
  log "Hello sailor!"
