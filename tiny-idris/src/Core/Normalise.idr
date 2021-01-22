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

argsFromStack :
  (args : List Name) ->
  Stack free ->
  Maybe (LocalEnv free args, Stack free)
argsFromStack [] stack = Just ([], stack)
argsFromStack (_ :: _) [] = Nothing
argsFromStack (n :: args) (closure :: oldStack) = do
  (localEnv, newStack) <- argsFromStack args oldStack
  pure (closure :: localEnv, newStack)

mutual
  eval :
    {free : List Name} ->
    {vars : List Name} ->
    Defs ->
    Env Term free ->
    LocalEnv free vars ->
    Term (vars ++ free) ->
    Stack free ->
    Core (NF free)
  eval defs env localEnv (Local p) stack =
    evalLocal defs env localEnv p stack
  eval defs env localEnv (Ref nt n) stack =
    evalRef defs env nt n stack (NApp (NRef nt n) stack)
  eval defs env localEnv (Meta x xs) stack = ?eval_rhs_3
  eval defs env localEnv (Bind n (Lam _ ty) scope) (x :: xs) = ?eval_rhs_4
  eval defs env localEnv (Bind n b scope) stack = ?eval_rhs_5
  eval defs env localEnv (App f x) stack = ?eval_rhs_6
  eval _ _ _ TType _ = pure Core.Value.NType
  eval _ _ _ Erased _ = pure Core.Value.NErased

  -- If it's one of the free variables, we are done
  -- (Idris 2 has Let bindings, which we'd need to check and evaluate here)
  evalLocal :
    {free : List Name} ->
    {vars : List Name} ->
    {idx : Nat} ->
    Defs ->
    Env Term free ->
    LocalEnv free vars -> -- List (Closure free)
    (0 _ : IsVar name idx (vars ++ free)) ->
    Stack free -> -- List (Closure free)
    Core (NF free)
  -- NOTE base case 1: IsVar points to a free variable
  evalLocal _ {vars = []} env localEnv isVar stack =
    pure $ NApp (NLocal isVar) stack
  -- NOTE base case 2: IsVar locates a bounded variable in LocalEnv
  evalLocal defs {vars = _ :: _} _ ((MkClosure localEnv env tm) :: _) First stack =
    eval defs env localEnv tm stack
  -- NOTE induction step
  evalLocal defs {vars = _ :: vs} env (_ :: ls) (Later isVar) stack =
    evalLocal defs {vars = vs} env ls isVar stack

  evalRef :
    {free : List Name} ->
    Defs ->
    Env Term free ->
    NameType ->
    Name ->
    Stack free ->
    (def : Lazy (NF free)) ->
    Core (NF free)
  evalRef defs env Func n stack def = do
    Just globalDef <- Core.Context.lookupDef n defs
      | Nothing => pure def
    evalDef env globalDef.definition stack def
  evalRef _ _ Bound _ _ def =
    pure def
  -- if it's a constructor, no need to look it up
  evalRef _ _ (DataCon tag arity) n stack _ =
    pure $ NDCon n tag arity stack
  evalRef _ _ (TyCon tag arity) n stack _ =
    pure $ NTCon n tag arity stack

  evalDef :
    {free : List Name} ->
    Env Term free ->
    Def ->
    Stack free ->
    (def : Lazy (NF free)) ->
    Core (NF free)
  evalDef env (PMDef args caseTree) oldStack def =
    case argsFromStack args oldStack of
      Nothing => pure def
      Just (localEnv, newStack) => do
        Result res <- evalTree env localEnv newStack caseTree
          | _ => pure def
        pure res
  evalDef _ _ _ def = pure def

  evalTree :
    {args : List Name} ->
    {free : List Name} ->
    Env Term free ->
    LocalEnv free args ->
    Stack free ->
    CaseTree args ->
    Core (CaseResult (NF free))

export
evalClosure :
  {free : List Name} ->
  Defs ->
  Closure free ->
  Core (NF free)
evalClosure defs (MkClosure locs env tm) =
  eval defs env locs tm []
