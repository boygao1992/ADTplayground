module Core.Env

import Core.TT
import Data.List

-- | NOTE Env
-- | Env : Type 2
-- |   (((List Name : Type 0) -> (Type 0 : Type 1)) : Type 1) ->
-- |   (List Name : Type 0) ->
-- |   (Type 1 : Type 2)
public export
data Env : (tm : List Name -> Type) -> List Name -> Type where
  Nil : Env tm []
  (::) : Binder (tm vars) -> Env tm vars -> Env tm (x :: vars)

{- | NOTE derivation

# base case

prove:
  reverseOnto xs [] = reverse [] ++ xs
  reverseOnto xs [] = reverseOnto [] [] ++ xs
  xs = [] ++ xs
  xs = xs

proof:
  Refl : xs = xs

# induction step

prove:
  reverseOnto xs (v :: vs) = reverse (v :: vs) ++ xs
  reverseOnto (v :: xs) vs = reverseOnto [] (v :: vs) ++ xs
  reverseOnto (v :: xs) vs = reverseOnto [v] vs ++ xs

proof: 3 . 2 . 1 in Refl

1: revOnto (v :: xs) vs:
  reverseOnto (v :: xs) vs = reverse vs ++ (v :: xs)
  reverseOnto (v :: xs) vs = reverseOnto [] vs ++ (v :: xs)

2: appendAssociative . (++) induction step . (++) base case:
  reverseOnto [] vs ++ (v :: xs) = (reverseOnto [] vs ++ [v]) ++ xs

  appendAssociative:
    reverseOnto [] vs ++ ([v] ++ xs) = (reverseOnto [] vs ++ [v]) ++ xs

  (++) induction step . (++) base case:
    [v] ++ xs = v :: xs

  (++) base case:
    [] ++ xs = xs

  (++) induction step:
    [v] ++ xs = v :: ([] ++ xs)

3: revOnto [v] vs:
  reverseOnto [v] vs = reverse vs ++ [v]
  reverseOnto [v] vs = reverseOnto [] vs ++ [v]

-}
revOnto :
  (xs : List Name) ->
  (vs : List Name) ->
  reverseOnto xs vs = reverse vs ++ xs
revOnto xs [] = Refl -- xs == xs
revOnto xs (v :: vs) =
  let
    prf1 = revOnto (v :: xs) vs
    prf2 = Data.List.appendAssociative (reverseOnto [] vs) [v] xs
    prf3 = revOnto [v] vs
  in
    -- reverseOnto (v :: xs) vs = reverseOnto [v] vs ++ xs
    rewrite prf1 in -- reverseOnto (v :: xs) vs = reverseOnto [] vs ++ (v :: xs)
      -- reverseOnto [] vs ++ (v :: xs) = reverseOnto [v] vs ++ xs
      rewrite prf2 in -- reverseOnto [] vs ++ (v :: xs) = (reverseOnto [] vs ++ [v]) ++ xs
        -- (reverseOnto [] vs ++ [v]) ++ xs = reverseOnto [v] vs ++ xs
        rewrite prf3 in -- reverseOnto [v] vs = reverseOnto [] vs ++ [v]
          -- (reverseOnto [] vs ++ [v]) ++ xs = (reverseOnto [] vs ++ [v]) ++ xs
          Refl
