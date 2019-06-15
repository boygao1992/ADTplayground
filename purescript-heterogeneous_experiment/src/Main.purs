module Main where

import Prelude

import Data.List ((:), List(Nil), reverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Data.Symbol (class IsSymbol, SProxy, reflectSymbol)

data RecordToList = RecordToList

instance recordToListReducerString
  :: ( IsSymbol label
    )
  => FoldingWithIndex RecordToList (SProxy label) (List (Tuple String String)) String (List (Tuple String String))
  where
    foldingWithIndex RecordToList label list a =
      (Tuple (reflectSymbol label) a) : list
else instance recordToListReducerNonString
  :: ( Show a
    , IsSymbol label
    )
  => FoldingWithIndex RecordToList (SProxy label) (List (Tuple String String)) a (List (Tuple String String))
  where
    foldingWithIndex RecordToList label list a =
      (Tuple (reflectSymbol label) (show a)) : list

recordToList
  :: forall r
  . HFoldlWithIndex RecordToList
      (List (Tuple String String))
      r
      (List (Tuple String String))
  => r
  -> List (Tuple String String)
recordToList
  = reverse <<< hfoldlWithIndex RecordToList (Nil :: List (Tuple String String))

sampleOutput :: List (Tuple String String)
sampleOutput
  = recordToList { a: 1, b: "2" }

main :: Effect Unit
main = do
  logShow sampleOutput
