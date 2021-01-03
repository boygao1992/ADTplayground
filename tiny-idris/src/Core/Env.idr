module Core.Env

import Core.TT
import Data.List

-- | NOTE Env
-- | Env : Type 2
-- |   (((List Name : Type 0) -> (Type 0 : Type 1)) : Type 1) ->
-- |   (List Name : Type 0) ->
-- |   (Type 1 : Type 2)
-- |
-- | [] : Env tm []
-- | [ ( _ : Binder (tm []) ) ] : Env tm [x]
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

proof:
  3 . 2 . 1 in Refl

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
  (xs : List a) ->
  (vs : List a) ->
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

{- | NOTE derivation

# base case

prove:
  reverse ys ++ [] = reverse ys
  reverseOnto [] ys ++ [] = reverseOnto [] ys

proof:
  1 or 2 in Refl

1: appendNilRightNeutral (reverseOnto [] ys):
  reverseOnto [] ys ++ [] = reverseOnto [] ys

2: revOnto [] ys:
  reverseOnto [] ys = reverse ys ++ []
  reverseOnto [] ys = reverseOnto [] ys ++ []

# induction step

prove:
  reverse ys ++ reverse (x :: xs) = reverse ((x :: xs) ++ ys)
  reverseOnto [] ys ++ reverseOnto [] (x :: xs) = reverseOnto [] (x :: (xs ++ ys))
  reverseOnto [] ys ++ reverseOnto [x] xs = reverseOnto [x] (xs ++ ys)

proof:
  4 . 3 . 2 . 1 in Refl

Refl:
  (reverseOnto [] ys ++ reverseOnto [] xs) ++ [x] = (reverseOnto [] ys ++ reverseOnto [] xs) ++ [x]

1: revNs xs ys:
  reverseOnto [] ys ++ reverseOnto [] xs = reverseOnto [] (xs ++ ys)

2: appendAssociative:
  reverseOnto [] ys ++ (reverseOnto [] xs ++ [x]) = (reverseOnto [] ys ++ reverseOnto [] xs) ++ [x]

3: revOnto [x] xs:
reverseOnto [x] xs = reverseOnto [] xs ++ [x]

4: revOnto [x] (xs ++ ys):
  reverseOnto [x] (xs ++ ys) = reverseOnto [] (xs ++ ys) ++ [x]

-}
revNs :
  (xs : List a) ->
  (ys : List a) ->
  reverse ys ++ reverse xs = reverse (xs ++ ys)
revNs [] ys =
  let
    prf1 = Data.List.appendNilRightNeutral (reverseOnto [] ys)
  in
    -- reverseOnto [] ys ++ [] = reverseOnto [] ys
    rewrite prf1 in -- reverseOnto [] ys ++ [] = reverseOnto [] ys
      -- reverseOnto [] ys = reverseOnto [] ys
      Refl
revNs (x :: xs) ys =
  let
    prf1 = revNs xs ys
    prf2 = Data.List.appendAssociative (reverseOnto [] ys) (reverseOnto [] xs) [x]
    prf3 = revOnto [x] xs
    prf4 = revOnto [x] (xs ++ ys)
  in
    -- reverseOnto [] ys ++ reverseOnto [x] xs = reverseOnto [x] (xs ++ ys)
    rewrite prf4 in -- reverseOnto [x] (xs ++ ys) = reverseOnto [] (xs ++ ys) ++ [x]
      -- reverseOnto [] ys ++ reverseOnto [x] xs = reverseOnto [] (xs ++ ys) ++ [x]
      rewrite prf3 in -- reverseOnto [x] xs = reverseOnto [] xs ++ [x]
        -- reverseOnto [] ys ++ (reverseOnto [] xs ++ [x]) = reverseOnto [] (xs ++ ys) ++ [x]
        rewrite prf2 in -- reverseOnto [] ys ++ (reverseOnto [] xs ++ [x]) = (reverseOnto [] ys ++ reverseOnto [] xs) ++ [x]
          -- (reverseOnto [] ys ++ reverseOnto [] xs) ++ [x] = reverseOnto [] (xs ++ ys) ++ [x]
          rewrite prf1 in -- reverseOnto [] ys ++ reverseOnto [] xs = reverseOnto [] (xs ++ ys)
            -- reverseOnto [] (xs ++ ys) ++ [x] = reverseOnto [] (xs ++ ys) ++ [x]
            Refl

-- Weaken by all the names at once at the end, to save multiple traversals
-- in big environments
-- Also reversing the names at the end saves significant time over concatenating
-- when environments get fairly big.
getBinderUnder :
  Weaken tm =>
  {idx : Nat} ->
  {vars : List Name} ->
  (ns : List Name) ->
  (0 _ : IsVar name idx vars) ->
  Env tm vars ->
  Binder (tm (reverseOnto vars ns))
getBinderUnder {vars = (name :: vs)} ns First (b :: env) =
  -- reverseOnto: reverseOnto (name :: vs) ns = reverseOnto vs (name :: ns)
  -- Binder (tm (reverseOnto (name :: vs) ns))
  rewrite revOnto vs (name :: ns) in -- reverseOnto vs (name :: ns) = reverse (name :: ns) ++ vs
    -- Binder (tm (reverse (name :: ns) ++ vs))
    map (weakenNs (reverse (name :: ns))) b
getBinderUnder {vars = (v :: _)} ns (Later isVar) (_ :: env) =
  getBinderUnder (v :: ns) isVar env

export
getBinder :
  Weaken tm =>
  {vars : List Name} ->
  {idx : Nat} ->
  (0 _ : IsVar name idx vars) ->
  Env tm vars ->
  Binder (tm vars)
getBinder isVar env = getBinderUnder [] isVar env

-- | NOTE IsDefined
-- | alias of Core.TT.NVar
public export
data IsDefined : Name -> List Name -> Type where
  MkIsDefined :
    {idx : Nat} ->
    (0 _ : IsVar name idx vars) ->
    IsDefined name vars

export
defined :
  {vars : List Name} ->
  (name : Name) ->
  Env tm vars ->
  Maybe (IsDefined name vars)
defined {vars = []} name [] = Nothing
defined {vars = (v :: vs)} name (_ :: env) =
  case nameEq v name of
    Nothing => do
      MkIsDefined p <- defined name env
      pure (MkIsDefined (Later p))
    Just Refl => -- v == name
      Just (MkIsDefined First)

-- Make a dummy environment, if we genuinely don't care about the values
-- and types of the contents.
-- We use this when building and comparing case trees.
export
mkEnv : (vs : List Name) -> Env Term vs
mkEnv [] = []
mkEnv (n :: ns) = PVar Erased :: mkEnv ns
