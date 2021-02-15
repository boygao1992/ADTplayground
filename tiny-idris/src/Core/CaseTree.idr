module Core.CaseTree

import Core.TT
import Data.List

-- NOTE The Implementation of Functional Programming Languages - CH5
mutual
  -- | NOTE Case Tree
  -- Case trees
  -- We may only dispatch on variables, not expressions
  public export
  data CaseTree : List Name -> Type where
    -- case split
    -- case x : scTy of { p1 => e1 ; ... }
    Case :
      {name : Name} ->
      {vars : List Name} ->
      {idx : Nat} ->
      (0 _ : IsVar name idx vars) ->
      (scTy : Term vars) ->
      List (CaseAlt vars) ->
      CaseTree vars
    -- expression
    -- RHS: no need for further inspection
    STerm : Term vars -> CaseTree vars
    -- error from a partial match
    Unmatched : (msg : String) -> CaseTree vars
    -- unreachable case
    -- Absurd context
    Impossible : CaseTree vars

  -- | NOTE CaseAlt (single-variable pattern lambda)
  -- Case alternatives. Unlike arbitrary patterns, they can be at most
  -- one constructor deep.
  -- Idris2 also needs cases for 'Delay' and primitives.
  public export
  data CaseAlt : List Name -> Type where
    -- constructor application
    -- Constructor for a data type; bind the arguments and subterms.
    ConCase :
      Name ->
      (tag : Int) ->
      (argNs : List Name) ->
      CaseTree (argNs ++ vars) ->
      CaseAlt vars
    -- match anything
    -- Catch-all case
    DefaultCase : CaseTree vars -> CaseAlt vars

mutual
  insertCaseNames :
    {outer : List Name} ->
    {inner : List Name} ->
    (ns : List Name) ->
    CaseTree (outer ++ inner) ->
    CaseTree (outer ++ (ns ++ inner))
  insertCaseNames {outer} {inner} ns (Case isVar scTy alts) =
    let
      MkNVar isVar' = Core.TT.insertNVarNames {outer} {middle = ns} {inner} isVar
      scTy' = Core.TT.insertNames {outer} {inner} ns scTy
      alts' = map (insertCaseAltNames {outer} {inner} ns) alts
    in
      Case isVar' scTy' alts'
  insertCaseNames ns (STerm x) = STerm (Core.TT.insertNames ns x)
  insertCaseNames _ (Unmatched msg) = Unmatched msg
  insertCaseNames _ Impossible = Impossible

  insertCaseAltNames :
    {outer : List Name} ->
    {inner : List Name} ->
    (ns : List Name) ->
    CaseAlt (outer ++ inner) ->
    CaseAlt (outer ++ (ns ++ inner))
  insertCaseAltNames {outer} {inner} ns (ConCase x tag argNs ct) =
    ConCase x tag argNs
      (rewrite Data.List.appendAssociative argNs outer (ns ++ inner) in
        insertCaseNames {outer = argNs ++ outer} {inner} ns
          (rewrite sym (Data.List.appendAssociative argNs outer inner) in ct)
      )
  insertCaseAltNames ns (DefaultCase x) =
    DefaultCase
      (insertCaseNames ns x)

export
Weaken CaseTree where
  weakenNs ns t = insertCaseNames {outer = []} ns t

-- NOTE 4.2.2 definition of patterns
-- Patterns, which arise from LHS expressions, and are converted to
-- case trees
public export
data Pat : Type where
  -- NOTE constructor pattern
  PCon :
    Name -> (tag : Int) -> (arity : Nat) ->
    List Pat -> -- arguments TODO size of List == arity?
    Pat
  -- NOTE variable
  PLoc : Name -> Pat
  -- NOTE invalid
  PUnmatchable : Term [] -> Pat

export
Show Pat where
  show (PCon n t a argNs) = show n ++ show (t, a) ++ show argNs
  show (PLoc n) = "{" ++ show n ++ "}"
  show _ = "_"

{- NOTE example

data List : Type -> Type where
  -- (0, 0)
  Nil : List a
  -- (1, 2)
  Cons : a -> List a -> List a

(Cons x1 Nil)

App (App (Ref (DataCon 1 2) "List") (Ref Bound "x1")) (Ref (DataCon 0 0) "List")

PCon "List" 1 2 [PLoc "x1", PCon "List" 0 0 []]
-}
export
mkPat' : List Pat -> Term [] -> Term [] -> Pat
mkPat' args _ (Ref Bound n) = PLoc n
mkPat' args _ (Ref (DataCon tag arity) n) = PCon n tag arity args
mkPat' args orig (App f x) =
  let
    parg = mkPat' [] x x
  in
    mkPat' (parg :: args) orig f
mkPat' _ orig _ = PUnmatchable orig

export
argToPat : Term [] -> Pat
argToPat tm = mkPat' [] tm tm

export
mkTerm : (vars : List Name) -> Pat -> Term vars
mkTerm vars (PCon n tag arity xs) =
  Core.TT.apply (Ref (DataCon tag arity) n)
    (map (mkTerm vars) xs)
mkTerm vars (PLoc n) = case Core.TT.isVar n vars of
  Nothing => Ref Bound n
  Just (MkVar isVar) => Local isVar
mkTerm vars (PUnmatchable tm) = Core.TT.embed tm

-- Show instances

mutual
  export
  {vars : _} -> Show (CaseTree vars) where
    show (Case {name} {idx} prf ty alts) =
      "case " ++ show name ++ "[" ++ show idx ++ "] : " ++ show ty ++ " of { "
      ++ showSep " | " (assert_total (map show alts)) ++ " }"
    show (STerm tm) = show tm
    show (Unmatched msg) = "Error: " ++ show msg
    show Impossible = "Impossible"

  export
  {vars : _} -> Show (CaseAlt vars) where
    show (ConCase n tag argNs sc) =
      show n ++ " " ++ showSep " " (map show argNs) ++ " => " ++ show sc
    show (DefaultCase sc) =
      "_ => " ++ show sc
