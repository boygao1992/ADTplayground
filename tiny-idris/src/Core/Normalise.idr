-- | NOTE normalization by evaluation
-- | eval : Term -> NF
-- | quote : NF -> Term
-- | normalize = quote <<< eval : Term -> Term
-- |
-- | [YOW! Lambda Jam 2020 - Edward Kmett - Cadenza Building Fast Functional Languages Fast](https://www.youtube.com/watch?v=25RmUl88jSw)
-- | Expr (Term)
-- |   = Var Name | App Expr Expr | Lam Name Expr
-- |   | LitBool Bool | If Expr Expr Expr
-- |   | Zero | Succ Expr | Rec Type Expr Expr Expr
-- |
-- | Type = Bool | Nat | Type :-> Type
-- |
---| Env a = [(Name, a)]
---|
-- | Value (NF)
-- |   = Closure (Env Value) Name Expr NOTE Lam in evaluation context
-- |   | Neutral Neutral
-- |   | VLitBool Bool
-- |   | VZero | VSucc Value
-- |
-- | Neutral
-- |   = NVar Name | NApp Type Neutral Value Value
-- |   | NIf Neutral Value Value
-- |   | NRec Type Neutral Value Value
-- |
-- | eval : Env Value -> Expr -> m Value
-- |
-- | uneval : [Name] -> Value -> Expr
-- |
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
  (argNs : List Name) ->
  Stack free ->
  Maybe (LocalEnv free argNs, Stack free)
argsFromStack [] stack = Just ([], stack)
argsFromStack (_ :: _) [] = Nothing
argsFromStack (n :: argNs) (closure :: oldStack) = do
  (localEnv, newStack) <- argsFromStack argNs oldStack
  pure (closure :: localEnv, newStack)

getCaseBound :
  List (Closure free) -> -- arguments in Closure
  (argNs : List Name) -> -- corresponding argument names
  LocalEnv free more ->
  Maybe (LocalEnv free (argNs ++ more))
getCaseBound [] [] localEnv = Just localEnv
getCaseBound [] (_ :: _) _ = Nothing -- mismatch arg length
getCaseBound (_ :: _) [] _ = Nothing -- mismatch arg length
getCaseBound (arg :: args) (n :: argNs) localEnv = do
  localEnvMore <- getCaseBound args argNs localEnv
  pure (arg :: localEnvMore)

mutual
  eval :
    {free : List Name} ->
    {bound : List Name} ->
    Defs -> -- NOTE global definitions, e.g. data/type constructor, functions, etc.
    Env Term free -> -- NOTE free variables from binders
    LocalEnv free bound -> -- NOTE bound variables (from lambda binding)
    Stack free -> -- NOTE arguments applied to top-level function
    Term (bound ++ free) -> -- NOTE term to be evaluated
    Core (NF free)
  eval defs env localEnv stack (Local p) =
    evalLocal defs env localEnv stack p
  eval defs env localEnv stack (Ref nt n) =
    evalRef defs env stack nt n
      (NApp (NRef nt n) stack)
  eval defs env localEnv stack (Meta n args) =
    let
      argClosures = map (MkClosure localEnv env) args
    in
      evalRef defs env (argClosures ++ stack) Func n
        (NApp (NMeta n argClosures) stack)
  eval defs env localEnv (arg :: stack) (Bind n (Lam _ _) scope) =
    eval defs env (arg :: localEnv) stack scope
  eval defs env localEnv stack (Bind n b scope) = do
    bNF <- traverse (\tm => eval defs env localEnv stack tm) b
    pure $
      NBind n bNF
        -- NOTE preserve (Stack free) for (NApp (NRef nt n) stack)
        -- in eval (Ref _ _)
        (\defs', arg =>
            eval defs' env (arg :: localEnv) stack scope
        )
  eval defs env localEnv stack (App f x) =
    eval defs env localEnv (MkClosure localEnv env x :: stack) f
  eval _ _ _ _ TType = pure Core.Value.NType
  eval _ _ _ _ Erased = pure Core.Value.NErased

  -- If it's one of the free variables, we are done
  -- (Idris 2 has Let bindings, which we'd need to check and evaluate here)
  evalLocal :
    {free : List Name} ->
    {bound : List Name} ->
    {idx : Nat} ->
    Defs ->
    Env Term free ->
    LocalEnv free bound -> -- List (Closure free)
    Stack free -> -- List (Closure free)
    (0 _ : IsVar name idx (bound ++ free)) ->
    Core (NF free)
  -- NOTE base case 1: IsVar points to a free variable
  evalLocal {bound = []} _ env localEnv stack isVar =
    pure $ NApp (NLocal isVar) stack
  -- NOTE base case 2: IsVar locates a bound variable in LocalEnv
  evalLocal {bound = _ :: _} defs _ ((MkClosure localEnv env tm) :: _) stack First =
    eval defs env localEnv stack tm
  -- NOTE induction step
  evalLocal {bound = _ :: bs} defs env (_ :: ls) stack (Later isVar) =
    evalLocal {bound = bs} defs env ls stack isVar

  evalRef :
    {free : List Name} ->
    Defs ->
    Env Term free ->
    Stack free ->
    NameType ->
    Name ->
    (def : Lazy (NF free)) ->
    Core (NF free)
  evalRef defs env stack Func n def = do
    Just globalDef <- Core.Context.lookupDef n defs
      | Nothing => pure def
    evalDef defs env globalDef.definition stack def
  evalRef _ _ _ Bound _ def =
    pure def
  -- if it's a constructor, no need to look it up
  evalRef _ _ stack (DataCon tag arity) n _ =
    pure $ NDCon n tag arity stack
  evalRef _ _ stack (TyCon tag arity) n _ =
    pure $ NTCon n tag arity stack

  evalDef :
    {free : List Name} ->
    Defs ->
    Env Term free ->
    Def ->
    Stack free ->
    (def : Lazy (NF free)) ->
    Core (NF free)
  -- PMDef = Pattern Matching Def
  -- ordinary function definition, e.g.
  --   f x y z = case x of { p1 => e1 ; ... }
  evalDef defs env (PMDef argNs caseTree) oldStack def =
    case argsFromStack argNs oldStack of
      Nothing => pure def
      Just (localEnv, newStack) => do
        Result res <- evalTree defs env localEnv newStack caseTree
          | _ => pure def
        pure res
  evalDef _ _ _ _ def = pure def

  evalTree :
    {argNs : List Name} ->
    {free : List Name} ->
    Defs ->
    Env Term free ->
    LocalEnv free argNs ->
    Stack free ->
    CaseTree argNs ->
    Core (CaseResult (NF free))
  -- NOTE case x of { p1 => e1; ... }
  -- NOTE `x` is a bound variable in (LocalEnv _ argNs)
  -- to evaluate which doesn't need (Stack free)
  -- so `evalLocal` takes an empty stack []
  evalTree defs env localEnv stack (Case isVar _ alts) = do
    varNF <- evalLocal defs env localEnv [] (Core.TT.varExtend isVar)
    -- Idris 2 also updates the local environment here, to save
    -- recomputing, but it involves a slightly trickier definition
    -- of closures, so we'll just carry on
    findAlt defs env localEnv stack varNF alts
  evalTree defs env localEnv stack (STerm tm) =
    Result <$>
      eval defs env localEnv stack (Core.TT.embed tm)
  evalTree _ _ _ _ (Unmatched _) = pure GotStuck
  evalTree _ _ _ _ Impossible = pure GotStuck

  findAlt :
    {free : List Name} ->
    {args : List Name} ->
    Defs ->
    Env Term free ->
    LocalEnv free args ->
    Stack free ->
    NF free -> -- value to match on
    List (CaseAlt args) -> -- list of pattern lambda
    Core (CaseResult (NF free))
  -- NOTE base case: exhausted all pattern lambdas
  findAlt _ _ _ _ _ [] = pure GotStuck
  -- NOTE induction step: proceed if current pattern lambda fails
  findAlt defs env localEnv stack val (x :: xs) = do
    NoMatch <- tryAlt defs env localEnv stack val x
      | Result x => pure (Result x)
      | GotStuck => pure GotStuck
    findAlt defs env localEnv stack val xs

  tryAlt :
    {free : List Name} ->
    {more : List Name} ->
    Defs ->
    Env Term free ->
    LocalEnv free more ->
    Stack free ->
    NF free ->
    CaseAlt more ->
    Core (CaseResult (NF free))
  -- Ordinary constructor matching, e.g.
  --   (case _ of
  --     X a b c => caseTree
  --   ) (X 1 2 3)
  --
  -- args = [1, 2, 3]
  -- argNs = ["a", "b", "c"]
  tryAlt defs env localEnv stack (NDCon dn dTag dArity args) (ConCase cn cTag argNs caseTree) =
    -- NOTE match value constructor with pattern constructor
    if dTag /= cTag
    then
      pure NoMatch
    else do
      -- NOTE name args by argNs and add them to LocalEnv
      let
        Just bound = getCaseBound args argNs localEnv
          | Nothing => pure GotStuck
      -- NOTE potentially further matching on any bound variables in LocalEnv
      -- including new ones from this data constructor
      evalTree defs env bound stack caseTree
  -- Default case matches against any *concrete* value
  tryAlt defs env localEnv stack val (DefaultCase caseTree) =
    if concrete val
    then
      evalTree defs env localEnv stack caseTree
    else
      pure GotStuck
    where
    concrete : NF free -> Bool
    concrete (NBind _ _ _) = True
    concrete (NDCon _ _ _ _) = True
    concrete (NTCon _ _ _ _) = True -- NOTE this is the only one from tiny-idris2, the rest from idris2
    concrete NType = True
    concrete _ = False
  tryAlt _ _ _ _ _ _ = pure GotStuck

export
evalClosure :
  {free : List Name} ->
  Defs ->
  Closure free ->
  Core (NF free)
evalClosure defs (MkClosure locs env tm) =
  eval defs env locs [] tm

export
nf :
  {free : List Name} ->
  Defs ->
  Env Term free ->
  Term free ->
  Core (NF free)
nf defs env tm = eval defs env [] [] tm

export
gnf :
  {free : List Name} ->
  Env Term free ->
  Term free ->
  Glued free
gnf env tm =
  MkGlue
    (pure tm)
    (\ctx => do
        defs <- Core.Core.get Ctxt
        nf defs env tm
    )

export
gType : Glued free
gType = MkGlue (pure TType) (const (pure NType))

-- | NOTE QVar
-- | supply (machine generated) names during quote/convert
-- | e.g. weakening (1 + 1) to (\x -> 1 + 1)
export
data QVar : Type where

genName :
  {auto _ : Ref QVar Int} ->
  String ->
  Core Name
genName name = do
  index <- Core.Core.get QVar
  Core.Core.put QVar (index + 1) -- NOTE QVar: bump counter
  pure (Core.TT.MN name index)

-- | NOTE Quote
public export
interface Quote (tm : List Name -> Type) where
  quoteGen :
    {vars : List Name} ->
    Ref QVar Int -> -- name supply
    Defs ->
    Env Term vars ->
    tm vars ->
    Core (Term vars)

  quote :
    {vars : List Name} ->
    Defs ->
    Env Term vars ->
    tm vars ->
    Core (Term vars)
  quote defs env tm = do
    q <- Core.Core.newRef QVar 0 -- NOTE QVar: initialize counter
    quoteGen q defs env tm

mutual
  quoteGenNF :
    {bound : List Name} ->
    {free : List Name} ->
    Ref QVar Int ->
    Defs ->
    Bounds bound ->
    Env Term free ->
    NF free ->
    Core (Term (bound ++ free))
  quoteGenNF q defs bounds env (NBind n bNF evalScope) = do
    var <- genName "qv" -- NOTE introduce a new bound variable
    b <- quoteBinder q defs bounds env bNF
    scope <-
      quoteGenNF q defs
        (Add n var bounds) -- NOTE add to bound variables
        env
        !(evalScope defs (toClosure env (Ref Bound var)))
        -- NOTE Ref Bound (MN _) why?
        -- clues:
        -- 1. evalScope/eval (Ref Bound var) = NApp (NRef Bound var) stack
        --   - evalScope provides stack (args)
        -- 2. quoteGenNF (NApp f args) -> quoteHead f, quoteArgs args
        -- 3. quoteHead (NRef Bound (MN n i)) -> findName bounds i
        -- 4. if findName bounds i = Just (MkVar p)
        --    then quoteHead (NRef Bound (MN n i)) = Local _ _
        -- NOTE in summary, initially introduced as a free variable
        -- and let quoteHead to figure out if it's bound variable
        -- TODO what if introduced as (Local var) since we know
        -- it binds to the last slot in (Bounds _)? And let evalLocal
        -- do the work instead of quoteHead > findName?
        -- Maybe findName is faster than evalLocal?
    pure (Bind n b scope)
  quoteGenNF q defs bounds env (NApp f args) =
    pure
      (Core.TT.apply
        !(quoteHead q defs bounds env f)
        !(quoteArgs q defs bounds env args)
      )
  quoteGenNF q defs bounds env (NDCon n tag arity args) = do
    pure
      (Core.TT.apply
        (Ref (DataCon tag arity) n)
        !(quoteArgs q defs bounds env args)
      )
  quoteGenNF q defs bounds env (NTCon n tag arity args) =
    pure
      (Core.TT.apply
        (Ref (TyCon tag arity) n)
        !(quoteArgs q defs bounds env args)
      )
  quoteGenNF _ _ _ _ NType = pure TType
  quoteGenNF _ _ _ _ NErased = pure Erased

  quoteArgs :
    {bound : List Name} ->
    {free : List Name} ->
    Ref QVar Int ->
    Defs ->
    Bounds bound ->
    Env Term free ->
    List (Closure free) ->
    Core (List (Term (bound ++ free)))
  quoteArgs q defs bounds env [] = pure []
  quoteArgs q defs bounds env (a :: args) =
    pure
      ( !(quoteGenNF q defs bounds env !(evalClosure defs a))
        :: !(quoteArgs q defs bounds env args)
      )

  quoteBinder :
    {bound : List Name} ->
    {free : List Name} ->
    Ref QVar Int ->
    Defs ->
    Bounds bound ->
    Env Term free ->
    Binder (NF free) ->
    Core (Binder (Term (bound ++ free)))
  quoteBinder q defs bounds env (Lam pi tyNF) = do
    ty <- quoteGenNF q defs bounds env tyNF
    pure (Lam pi ty)
  quoteBinder q defs bounds env (Pi pi tyNF) = do
    ty <- quoteGenNF q defs bounds env tyNF
    pure (Pi pi ty)
  quoteBinder q defs bounds env (PVar tyNF) = do
    ty <- quoteGenNF q defs bounds env tyNF
    pure (PVar ty)
  quoteBinder q defs bounds env (PVTy tyNF) = do
    ty <- quoteGenNF q defs bounds env tyNF
    pure (PVTy ty)

  quoteHead :
    {bound : List Name} ->
    {free : List Name} ->
    Ref QVar Int ->
    Defs ->
    Bounds bound ->
    Env Term free ->
    NHead free ->
    Core (Term (bound ++ free))
  quoteHead {bound} _ _ _ _ (NLocal isVar) =
    let (MkVar isVar') = weakenNs bound (MkVar isVar)
    in pure (Local isVar')
  quoteHead q defs bounds env (NRef Bound (MN n i)) =
    case findName bounds of
      Nothing => pure (Ref Bound (MN n i))
      Just (MkVar isVar) => pure (Local (Core.TT.varExtend isVar))
    where
    findName : Bounds bound' -> Maybe (Var bound')
    findName None = Nothing
    findName (Add new (MN n' i') bs) =
      -- this uniquely identifies it, given how we
      -- generated the names, and is a faster test!
      if i == i'
      then Just (MkVar First)
      else do
        MkVar isVar <- findName bs
        pure (MkVar (Later isVar))
    findName (Add _ _ bs) = do
      MkVar isVar <- findName bs
      pure (MkVar (Later isVar))
  quoteHead q defs bounds env (NRef nt n) = pure (Ref nt n)
  quoteHead q defs bounds env (NMeta x args) =
    pure (Meta x !(quoteArgs q defs bounds env args))

export
Quote NF where
  quoteGen q defs env tm = quoteGenNF q defs None env tm

export
Quote Term where
  quoteGen q defs env tm = pure tm

export
Quote Closure where
  quoteGen q defs env c = quoteGen q defs env !(evalClosure defs c)

export
glueBack :
  {free : List Name} ->
  Defs ->
  Env Term free ->
  NF free ->
  Glued free
glueBack defs env nf =
  MkGlue
    (do
      empty <- Core.Context.clearDefs defs
      quote empty env nf
    )
    (\_ => pure nf)

-- | NOTE normalization by evaluation
export
normalise :
  {free : List Name} ->
  Defs ->
  Env Term free ->
  Term free ->
  Core (Term free)
normalise defs env tm = quote defs env !(nf defs env tm)

public export
interface Convert (tm : List Name -> Type) where
  convGen :
    {vars : List Name} ->
    Ref QVar Int ->
    Defs ->
    Env Term vars ->
    tm vars ->
    tm vars ->
    Core Bool

  convert :
    {vars : List Name} ->
    Defs ->
    Env Term vars ->
    tm vars ->
    tm vars ->
    Core Bool
  convert defs env tm tm' = do
    q <- Core.Core.newRef QVar 0
    convGen q defs env tm tm'
mutual
  allConv :
    {vars : List Name} ->
    Ref QVar Int ->
    Defs ->
    Env Term vars ->
    List (Closure vars) ->
    List (Closure vars) ->
    Core Bool
  allConv q defs env [] [] = pure True
  allConv q defs env (x :: xs) (y :: ys) =
    pure (!(convGen q defs env x y) && !(allConv q defs env xs ys))
  allConv q defs env _ _ = pure False

  chkConvHead :
    {vars: List Name} ->
    Ref QVar Int ->
    Defs ->
    Env Term vars ->
    NHead vars ->
    NHead vars ->
    Core Bool
  chkConvHead q defs env (NLocal {idx = x} _) (NLocal {idx = y} _) = pure (x == y)
  chkConvHead q defs env (NRef _ x) (NRef _ y) = pure (x == y)
  chkConvHead q defs env (NMeta x xs) (NMeta y ys) =
    if x == y
    then allConv q defs env xs ys
    else pure False
  chkConvHead q defs env _ _ = pure False

  convBinders :
    {vars : List Name} ->
    Ref QVar Int ->
    Defs ->
    Env Term vars ->
    Binder (NF vars) ->
    Binder (NF vars) ->
    Core Bool
  convBinders q defs env (Lam _ x) (Lam _ y) = convGen q defs env x y
  convBinders q defs env (Pi _ x) (Pi _ y) = convGen q defs env x y
  convBinders q defs env _ _ = pure False

  export
  Convert NF where
    convGen q defs env (NBind _ bx sx) (NBind _ by sy) = do
      var <- genName "conv"
      let c = MkClosure [] env (Ref Bound var)
      if !(convBinders q defs env bx by)
        then convGen q defs env !(sx defs c) !(sy defs c)
        else pure False
    -- NOTE eta conversion
    -- f
    -- \nx -> f nx
    convGen q defs env tmx@(NBind nx (Lam ix tx) sx) tmy = do
      empty <- Core.Context.clearDefs defs
      etay <-
        nf defs env
          (Bind nx (Lam ix !(quote defs env tx))
            (App (Core.TT.weaken !(quote defs env tmy)) -- TODO why weaken?
              (Local First)
            )
          )
      convGen q defs env tmx etay
    convGen q defs env tmx tmy@(NBind ny (Lam iy ty) sy) = do
      empty <- Core.Context.clearDefs defs
      etax <-
        nf defs env
          (Bind ny (Lam iy !(quote defs env ty))
            (App (Core.TT.weaken !(quote defs env tmx))
              (Local First)
            )
          )
      convGen q defs env etax tmy
    convGen q defs env (NApp x xs) (NApp y ys) =
      pure (!(chkConvHead q defs env x y) && !(allConv q defs env xs ys))
    convGen q defs env (NDCon nx _ _ xs) (NDCon ny _ _ ys) =
      pure (nx == ny && !(allConv q defs env xs ys))
    convGen q defs env (NTCon nx _ _ xs) (NTCon ny _ _ ys) =
      pure (nx == ny && !(allConv q defs env xs ys))
    convGen q defs env NType NType = pure True
    convGen q defs env NErased _ = pure True
    convGen q defs env _ NErased = pure True
    convGen q defs env _ _ = pure False

  export
  Convert Term where
    convGen q defs env x y =
      convGen q defs env !(nf defs env x) !(nf defs env y)

  export
  Convert Closure where
    convGen q defs env x y =
      convGen q defs env !(evalClosure defs x) !(evalClosure defs y)
