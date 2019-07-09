module Data.List.Utils
( takeAway
, batching
) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.List (List(..), (:), reverse)
import Data.List as List

-- | Utils

takeAway :: forall a. Int -> List a -> { result :: List a, rest :: List a }
takeAway = go Nil
  where
    go acc n rest | n < 1 = { result: reverse acc, rest }
    go acc _ Nil = { result: reverse acc, rest: Nil }
    go acc n (x : xs) = go (x : acc) (n - 1) xs

batching :: forall a. Int -> List a -> List (List a)
batching n input
  | n < 1 = pure input
  | otherwise = List.reverse $ tailRec go {acc: List.Nil, rest: input }
    where
      go
        :: { acc :: List (List a)
           , rest :: List a
           }
        -> Step
            { acc :: List (List a)
            , rest :: List a
            }
            (List (List a))
      go { acc, rest: Nil } = Done acc
      go { acc, rest } =
        let
          { result, rest } = takeAway n rest
        in
          Loop
            { acc: (result : acc)
            , rest
            }
