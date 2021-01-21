module Core.Normalise

import Core.CaseTree
import Core.Context
import Core.Core
import Core.Env
import Core.TT
import Core.Value
import Data.Nat
import Data.Vect

-- | NOTE Glued
-- | an alternative to TT.Term and Value.NF in (Env tm vars)
-- A pair of a term and its normal form. This could be constructed either
-- from a term (via 'gnf') or a normal form (via 'glueBack') but the other
-- part will only be constructed when needed, because it's in Core.
public export
data Glued : List Name -> Type where
  MkGlue :
    Core (Term vars) -> -- term
    (Ref Ctxt Defs -> Core (NF vars)) -> -- normal form
    Glued vars

export
getTerm : Glued vars -> Core (Term vars)
getTerm (MkGlue tm _) = tm

export
getNF : {auto c : Ref Ctxt Defs} -> Glued vars -> Core (NF vars)
getNF (MkGlue _ nf) = nf c

-- | NOTE Stack
Stack : List Name -> Type
Stack vars = List (Closure vars)

export
toClosure : Env Term outer -> Term outer -> Closure outer
toClosure env tm = MkClosure [] env tm

-- | NOTE CaseResult
-- | 4.2.4 Multiple Equations and Failure
-- |
-- | enriched lambda calculus expression:
-- |   f = \x ->
-- |         (\p1 -> E1) x
-- |     <|> (\p2 -> E2) x
-- |     ...
-- |     <|> (\pn -> En) x
-- |     <|> Error
-- |
-- | semantics equations for (<|>):
-- |   Fail   <|> b = b
-- |   Bottom <|> b = Bottom
-- |   a      <|> b = a
data CaseResult a
  = Result a
  | NoMatch -- Fail, case alternative didn't match anything
  | GotStuck -- Bottom, alternative matched, but got stuck later

mutual
  export
  evalClosure :
    {free : List Name} ->
    Defs ->
    Closure free ->
    Core (NF free)
  evalClosure defs (MkClosure locs env tm)
    = eval defs env locs tm []

  eval :
    {free : List Name} ->
    {vars : List Name} ->
    Defs ->
    Env Term free ->
    LocalEnv free vars ->
    Term (vars ++ free) ->
    Stack free ->
    Core (NF free)
  eval defs env localEnv (Local i p) stack = ?eval_rhs_1
  eval defs env localEnv (Ref x y) stack = ?eval_rhs_2
  eval defs env localEnv (Meta x xs) stack = ?eval_rhs_3
  eval defs env localEnv (Bind n (Lam _ ty) scope) (x :: xs) = ?eval_rhs_4
  eval defs env localEnv (Bind n b scope) stack = ?eval_rhs_5
  eval defs env localEnv (App f x) stack = ?eval_rhs_6
  eval _ _ _ TType _ = pure Core.Value.NType
  eval _ _ _ Erased _ = pure Core.Value.NErased

  parameters (defs : Defs)
    evalLocClosure :
      {free : List Name} ->
      Env Term free ->
      Stack free ->
      Closure free ->
      Core (NF free)

