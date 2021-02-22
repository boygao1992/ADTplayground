-- | NOTE The Implementation of Functional Programming Languages
-- | 11.3.1 Weak Head Normal Form
-- |
-- | Original expression (Term)
-- |   -- Normal order reductions of top-level redexes ->
-- | Weak Head normal form (no top-level redexes)
-- |   -- Normal order reductions of inner redexes ->
-- | Normal form (no redexes at all)
module Core.Value

import Core.Core
import Core.Context
import Core.Env
import Core.TT

mutual
  -- | NOTE LocalEnv
  public export
  data LocalEnv : List Name -> List Name -> Type where
    Nil : LocalEnv free []

    (::) :
      Closure free ->
      LocalEnv free vars ->
      LocalEnv free (x :: vars)

  -- | NOTE Closure
  public export
  data Closure : List Name -> Type where
    MkClosure :
      {bound : List Name} ->
      LocalEnv free bound -> -- named (Closure free)s, bound variables
      Env Term free -> -- named (Binder (Term _))s, free variables
      Term (bound ++ free) ->
      Closure free

-- | NOTE NHead, top-level of weak head normal form
-- The head of a value: things you can apply arguments to
public export
data NHead : List Name -> Type where
  NLocal :
    {idx : Nat} ->
    (0 _ : IsVar name idx vars) ->
    NHead vars

  NRef :
    NameType ->
    Name ->
    NHead vars

  NMeta :
    Name ->
    List (Closure vars) ->
    NHead vars

-- | NOTE NF, weak head normal form (WHNF), no top-level redexes
-- | NOTE alternative to (Term : List Name -> Type) in (Env tm vars)
-- Values themselves. 'Closure' is an unevaluated thunk, which means
-- we can wait until necessary to reduce constructor arguments
public export
data NF : List Name -> Type where
  NBind :
    Name ->
    Binder (NF vars) ->
    (Defs -> Closure vars -> Core (NF vars)) ->
    NF vars

  NApp :
    NHead vars ->
    List (Closure vars) ->
    NF vars

  NDCon :
    Name ->
    (tag : Int) ->
    (arity : Nat) ->
    List (Closure vars) ->
    NF vars

  NTCon :
    Name ->
    (tag : Int) ->
    (arity : Nat) ->
    List (Closure vars) ->
    NF vars

  NType : NF vars

  NErased : NF vars
