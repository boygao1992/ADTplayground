module StringDistance where

import Prelude

ls :: String -> String -> Int
ls "" "" = 0
ls s1 "" = length s1
ls "" s2 = length s2
ls s1 s2
  | head s1 == head s2 = ls (tail s1) (tail s2)
  | otherwise = minimum [ ls (tail s1) s2 + 1
                        , ls s1 (tail s2) + 1
                        , ls (tail s1) (tail s2) + 1]
