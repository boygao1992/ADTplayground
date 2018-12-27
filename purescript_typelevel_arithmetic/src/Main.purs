module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Type.Data.Boolean (BProxy, True, False)
import Type.Data.Symbol (SProxy(..))
import Utils (removeSpace, reverseSymbol)
import Digit (isDigit)
import Number (isNumber)

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

isDigitExample1 :: BProxy False
isDigitExample1 = isDigit $ SProxy :: SProxy ""

isDigitExample2 :: BProxy True
isDigitExample2 = isDigit $ SProxy :: SProxy "2"

isNumberExample1 :: BProxy False
isNumberExample1 = isNumber $ SProxy :: SProxy ""

isNumberExample2 :: BProxy True
isNumberExample2 = isNumber $ SProxy :: SProxy "123"

isNumberExample3 :: BProxy False
isNumberExample3 = isNumber $ SProxy :: SProxy "298723958-04"

main :: Effect Unit
main = do
  log "Hello sailor!"
