module Core.TT

import Data.List
import Decidable.Equality

-- | NOTE Name
public export
data Name : Type where
  UN : String -> Name -- user written name
  MN : String -> Int -> Name -- machine generated name

export
nameEq : (x : Name) -> (y : Name) -> Maybe (x = y)
nameEq (UN x) (UN y)
  with (Decidable.Equality.decEq x y)
  nameEq (UN y) (UN y) | Yes Refl = Just Refl
  nameEq (UN x) (UN y) | No _ = Nothing
nameEq (MN xName xIndex) (MN yName yIndex)
  with (Decidable.Equality.decEq xName yName)
  nameEq (MN _ xIndex) (MN _ yIndex) | Yes Refl
    with (Decidable.Equality.decEq xIndex yIndex)
    nameEq (MN _ _) (MN _ _) | Yes Refl | Yes Refl = Just Refl
    nameEq (MN _ _) (MN _ _) | Yes Refl | No _ = Nothing
  nameEq (MN _ _) (MN _ _) | No _ = Nothing
nameEq _ _ = Nothing

export
Eq Name where
  (==) (UN x) (UN y) = x == y
  (==) (MN xName xIndex) (MN yName yIndex) = xName == yName && xIndex == yIndex
  (==) _ _ = False

nameTag : Name -> Int
nameTag (UN _) = 0
nameTag (MN _ _) = 1

export
Ord Name where
  compare (UN x) (UN y) = compare x y
  compare (MN xName xIndex) (MN yName yIndex) = case compare xName yName of
    EQ => compare xIndex yIndex
    ordering => ordering
  compare x y = compare (nameTag x) (nameTag y)

-- | NOTE NameType
public export
data NameType : Type where
  Func : NameType
  Bound : NameType
  DataCon : (tag : Int) -> (arity : Nat) -> NameType
  TyCon : (tag : Int) -> (arity : Nat) -> NameType

export
Show NameType where
  show Func = "Func"
  show (DataCon t a) = "DataCon " ++ show (t, a)
  show (TyCon t a) = "TyCon " ++ show (t, a)
  show Bound = "Bound"

-- | NOTE IsVar | i ns -> n
-- | a proof that a de Bruijn index refers to a name in scope
public export
data IsVar : Name -> Nat -> List Name -> Type where
  First : IsVar n Z (n :: ns)
  Later : IsVar n i ns -> IsVar n (S i) (m :: ns)

public export
dropVar :
  {idx : Nat} ->
  (ns : List Name) ->
  (0 _ : IsVar n idx ns) ->
  List Name
dropVar (n :: ns) First = ns
dropVar (m :: ns) (Later p) = m :: dropVar ns p

export
varExtend : IsVar x idx outer -> IsVar x idx (outer ++ inner)
varExtend First = First
varExtend (Later y) = Later (varExtend y)

-- | NOTE Var
public export
data Var : List Name -> Type where
  MkVar : {i : Nat} -> (0 _ : IsVar n i ns) -> Var ns

export
dropFirst : List (Var (n :: ns)) -> List (Var ns)
dropFirst [] = []
dropFirst ((MkVar First) :: vs) = dropFirst vs
dropFirst ((MkVar (Later p)) :: vs) = MkVar p :: dropFirst vs

export
isVar : (n : Name) -> (ns : List Name) -> Maybe (Var ns)
isVar n [] = Nothing
isVar n (x :: xs) = case n == x of
  False => do
    MkVar p <- isVar n xs
    pure (MkVar (Later p))
  True =>
    pure (MkVar First)

-- | NOTE NVar
public export
data NVar : Name -> List Name -> Type where
  MkNVar : {i : Nat} -> (0 _ : IsVar n i ns) -> NVar n ns

export
weakenNVar :
  {idx : Nat} ->
  (outer : List Name) ->
  (0 _ : IsVar name idx inner) ->
  NVar name (outer ++ inner)
weakenNVar [] p = MkNVar p
weakenNVar (n :: ns) p =
  let
    MkNVar p' = weakenNVar ns p
  in
    MkNVar (Later p')

-- | NOTE PiInfo
public export
data PiInfo : Type where
  Implicit : PiInfo
  Explicit : PiInfo

-- | NOTE Binder
public export
data Binder : Type -> Type where
  Lam : PiInfo -> ty -> Binder ty
  Pi : PiInfo -> ty -> Binder ty

  PVar : ty -> Binder ty -- pattern bound variables ...
  PVTy : ty -> Binder ty -- ... and their types

export
binderType : Binder tm -> tm
binderType (Lam _ ty) = ty
binderType (Pi _ ty) = ty
binderType (PVar ty) = ty
binderType (PVTy ty) = ty

export
Functor Binder where
  map func (Lam x ty) = Lam x (func ty)
  map func (Pi x ty) = Pi x (func ty)
  map func (PVar ty) = PVar (func ty)
  map func (PVTy ty) = PVTy (func ty)

-- | NOTE Term
public export
data Term : List Name -> Type where
  Local :
    (i : Nat) -> -- de Bruijn index
    (0 p : IsVar n i ns) -> -- proof that index is valid
    Term ns
  Ref : NameType -> Name -> Term ns -- a reference to a global name
  Meta : Name -> List (Term ns) -> Term ns -- meta variable
  Bind :
    (n : Name) -> -- any binder, e.g. lambda or pi
    Binder (Term ns) ->
    (scope : Term (n :: ns)) -> -- one more name in scope
    Term ns
  App : Term ns -> Term ns -> Term ns -- function application
  TType : Term ns -- type of types
  Erased : Term ns -- place holder

-- | NOTE Weaken
public export
interface Weaken (tm : List Name -> Type) where
  weaken : {n: Name} -> {ns: List Name} -> tm ns -> tm (n :: ns)
  weakenNs :
    {inner: List Name} ->
    (outer : List Name) ->
    tm inner -> tm (outer ++ inner)

  weakenNs [] t = t
  weakenNs (n :: ns) t = weaken {n} (weakenNs ns t)

  weaken {n} = weakenNs [n]

export
Weaken Var where
  weaken (MkVar x) = MkVar (Later x)

-----------------------
-- Term manipulation --
-----------------------

-- apply a higher-order function to a list of arguments
export
apply : Term ns -> List (Term ns) -> Term ns
apply fn [] = fn
apply fn (a :: args) = apply (App fn a) args

-- get the list of arguments applied to a higher-order function
export
getFnArgs : Term ns -> (Term ns, List (Term ns))
getFnArgs tm = go [] tm
  where
  go :
    List (Term ns) ->
    Term ns ->
    (Term ns, List (Term ns))
  go args (App f x) = go (x :: args) f
  go args tm = (tm, args)

export
embed : Term outer -> Term (outer ++ inner)
embed (Local i p) =
  Local _ (varExtend p)
embed (Ref nt n) = Ref nt n
embed (Meta n args) =
  Meta n
    (map embed args)
embed (Bind n b scope) =
  Bind n
    (map embed b)
    (embed {outer = n :: outer} scope)
embed (App f x) =
  App
    (embed f)
    (embed x)
embed TType = TType
embed Erased = Erased

-- insert a list of name into the middle and shift index of the variable accordingly
export
insertNVarNames :
  {outer : List Name} ->
  {middle : List Name} ->
  (idx : Nat) ->
  (0 _ : IsVar name idx (outer ++ inner)) ->
  NVar name (outer ++ (middle ++ inner))
insertNVarNames {outer = []} {middle} idx p =
  weakenNVar middle p
insertNVarNames {outer = n :: ns} {middle} Z First =
  MkNVar (First {n} {ns = ns ++ (middle ++ inner)})
insertNVarNames {outer = n :: ns} {middle} (S i) (Later p) =
  let
    MkNVar p' = insertNVarNames {outer = ns} {middle} i p
  in
    MkNVar (Later p')

export
insertNames :
  {outer : List Name} ->
  {inner : List Name} ->
  (middle : List Name) ->
  Term (outer ++ inner) ->
  Term (outer ++ (middle ++ inner))
insertNames {outer} middle (Local i p) =
  let
    MkNVar p' = insertNVarNames {outer} {middle} i p
  in
    Local _ p'
insertNames {outer} {inner} middle (Ref nt n) =
  Ref {ns = outer ++ (middle ++ inner)} nt n
insertNames {outer} {inner} middle (Meta n args) =
  Meta n (map (insertNames {outer} {inner} middle) args)
insertNames {outer} {inner} middle (Bind n b scope) =
  Bind n
    (map (insertNames {outer} {inner} middle) b)
    (insertNames {outer = n :: outer} {inner} middle scope)
insertNames {outer} {inner} middle (App f x) =
  App
    (insertNames {outer} {inner} middle f)
    (insertNames {outer} {inner} middle x)
insertNames {outer} {inner} middle TType =
  TType {ns = outer ++ (middle ++ inner)}
insertNames {outer} {inner} middle Erased =
  Erased {ns = outer ++ (middle ++ inner)}

export
Weaken Term where
  weakenNs {inner} middle x = insertNames {outer = []} {inner} middle x

namespace Bounds
  -- | NOTE Bounds
  public export
  data Bounds : List Name -> Type where
    None : Bounds []
    Add : (n : Name) {- new -} -> Name {- old -} -> Bounds ns -> Bounds (n :: ns)

export
addVars :
  {later : List Name} ->
  {bound : List Name} ->
  {idx : Nat} ->
  Bounds bound ->
  (0 _ : IsVar name idx (later ++ vars)) ->
  NVar name (later ++ (bound ++ vars))
addVars {later} {bound} {idx} _ p =
  insertNVarNames {outer = later} {middle = bound} idx p

-- | Initially all references are global references (Ref) but we can turn
-- | some of them into local references to variables in the scope by
-- | resolving Lambda bindings.
-- |
-- | Implicit
-- |
-- | ```idris2
-- | f :: { old : _ } -> _
-- | f { old = new } = _
-- | ```
-- |
-- | Explicit
-- |
-- | ```idris2
-- | f :: (old : _) -> _
-- | f new = _
-- | ```
resolveRef :
  {outer : List Name} ->
  (done : List Name) ->
  Bounds bound ->
  Name ->
  Maybe (Term (outer ++ (done ++ (bound ++ vars))))
resolveRef _ None _ = Nothing
resolveRef {outer} {vars} done (Add {ns} new old bs) n =
  if n == old then
    -- Maybe (Term (outer ++ (done ++ (new :: (ns ++ vars)))))
    rewrite Data.List.appendAssociative outer done (new :: (ns ++ vars)) in
      -- Maybe (Term ((outer ++ done) ++ (new :: (ns ++ vars))))
      let
        MkNVar p = weakenNVar {inner = new :: (ns ++ vars)} (outer ++ done) First
      in
        Just (Local _ p)
  else
    -- Maybe (Term (outer ++ (done ++ ([new] ++ (ns ++ vars)))))
    rewrite Data.List.appendAssociative done [new] (ns ++ vars) in
      -- Maybe (Term (outer ++ ((done ++ [new]) ++ (ns ++ vars))))
      resolveRef {outer} {vars} (done ++ [new]) bs n
