module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Type.Data.Boolean (BProxy, True, False)
import Type.Data.Symbol (SProxy(..))
import Utils (removeSpace, reverseSymbol)
import Digit (isDigitPred)
import Int (isIntPred, add) as Int

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

isIntPredExample1 :: BProxy False
isIntPredExample1 = Int.isIntPred $ SProxy :: SProxy ""

isIntPredExample2 :: BProxy True
isIntPredExample2 = Int.isIntPred $ SProxy :: SProxy "123"

isIntPredExample3 :: BProxy False
isIntPredExample3 = Int.isIntPred $ SProxy :: SProxy "298723958-04"

addIntExample1 :: SProxy "10000"
addIntExample1 = Int.add (SProxy :: SProxy "1") (SProxy :: SProxy "9999")

addIntExample2 :: SProxy "1999998"
addIntExample2 = Int.add (SProxy :: SProxy "999999") (SProxy :: SProxy "999999")

addIntExample3 :: SProxy "968623218844"
addIntExample3 = Int.add (SProxy :: SProxy "8942758984") (SProxy :: SProxy "959680459860")

main :: Effect Unit
main = do
  log "Hello sailor!"
