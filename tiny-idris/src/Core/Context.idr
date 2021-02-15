module Core.Context

import Core.CaseTree
import Core.Core
import Core.Env
import Core.TT
import Data.SortedMap

-- | NOTE Def
public export
data Def : Type where
  -- not yet defined
  None : Def
  -- ordinary function definition, e.g.
  --   f x y z = case x of { p1 => e1 ; ... }
  PMDef :
    (args : List Name) ->
    (treeCT : CaseTree args) ->
    Def
  -- data constructor
  DCon :
    (tag : Int) ->
    (arity : Nat) ->
    Def
  -- type constructor
  TCon :
    (tag : Int) ->
    (arity : Nat) ->
    Def
  Hole : Def
  Guess :
    (guess : Term []) ->
    (constraints : List Int) -> -- unification constraints
    Def

-- | NOTE GlobalDef
public export
record GlobalDef where
  constructor MkGlobalDef
  type : Term []
  definition : Def

-- | NOTE Defs
-- | type alias
-- A mapping from names to definitions
-- Again there's more to this in Idris 2 since we need to deal with namespaces,
-- and there's also an array to map "resolved" name ids, which is much faster
-- for name lookups in general.
export
Defs : Type
Defs = SortedMap Name GlobalDef

-- This doesn't actually need to be in Core in this system, but it is in
-- Idris 2, because:
--  * the context is an IO array underneath, for O(1) lookup
--  * definitions can be updated on lookup, since we actually store things
--    as a binary encoded form that's stored on disk, and only decode when
--    first used.
-- So, it's in Core here so that there's a more clear mapping to the full
-- version.
export
lookupDef : Name -> Defs -> Core (Maybe GlobalDef)
lookupDef n defs = pure (Data.SortedMap.lookup n defs)

export
initDefs : Core Defs
initDefs = pure empty

export
clearDefs : Defs -> Core Defs
clearDefs _ = pure empty

-- A phantom type for finding references to the context
export
data Ctxt : Type where

-- A program consists of a series of data declarations, function type
-- declarations, and function clauses. Even in full Idris 2, this is what
-- everything translates down to. The following types represent well-type
-- data declarations and clauses, ready for adding to the context.

public export
record Constructor where
  constructor MkCon
  name : Name
  arity : Nat
  type : Term []

-- Well typed data declaration, e.g.
--
--   data A : (b : B) -> Type where
--     X : A b
--
--   MkData
--     (MkCon "A" 1 (Bind "b" (Lam _ (Ref (TyCon _ 0) "B")) Type))
--     [ MkCon "X" (Ref (TyCon _ 1) "A")]
public export
data DataDef : Type where
  MkData :
    (tyCon : Constructor) ->
    (dataCons : List Constructor) ->
    DataDef

-- A well typed pattern clause
public export
data Clause : Type where
  MkClauses :
    {var : List Name} ->
    (env : Env Term vars) ->
    (lhs : Term vars) ->
    (rhs : Term vars) ->
    Clause

-- Add (or replace) a definition
export
addDef :
  {auto _ : Ref Ctxt Defs} ->
  Name ->
  GlobalDef ->
  Core ()
addDef name def = do
  defs <- Core.Core.get Ctxt
  Core.Core.put Ctxt (Data.SortedMap.insert name def defs)

export
updateDef :
  {auto _ : Ref Ctxt Defs} ->
  Name ->
  (GlobalDef -> GlobalDef) ->
  Core ()
updateDef name f = do
  defs <- Core.Core.get Ctxt
  Just gDef <- lookupDef name defs
    | Nothing => throw (Core.Core.UndefinedName name)
  addDef name (f gDef)
