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

  -- | NOTE CaseAlt
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
      (args : List Name) ->
      CaseTree (args ++ vars) ->
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
  insertCaseAltNames {outer} {inner} ns (ConCase x tag args ct) =
    ConCase x tag args
      (rewrite Data.List.appendAssociative args outer (ns ++ inner) in
        insertCaseNames {outer = args ++ outer} {inner} ns
          (rewrite sym (Data.List.appendAssociative args outer inner) in ct)
      )
  insertCaseAltNames ns (DefaultCase x) =
    DefaultCase
      (insertCaseNames ns x)

export
Weaken CaseTree where
  weakenNs ns t = insertCaseNames {outer = []} ns t
