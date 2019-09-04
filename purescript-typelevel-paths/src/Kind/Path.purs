module Kind.Path where

import Prelude
import Type.Prelude

import Type.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Type.Data.Boolean as B

foreign import kind Constant
foreign import data CString :: Symbol -> Constant
foreign import data CInt :: Symbol -> Constant
foreign import data CBool :: B.Boolean -> Constant
data ConstantProxy (constant :: Constant) = ConstantProxy

foreign import kind Lit
foreign import data Var :: Symbol -> Lit
foreign import data Constant :: Constant -> Lit
data LitProxy (lit :: Lit) = LitProxy

foreign import kind Pred
foreign import data Eq :: Lit -> Pred
foreign import data Gt :: Lit -> Pred
foreign import data Lt :: Lit -> Pred
foreign import data Not :: Pred -> Pred
foreign import data And :: Pred -> Pred -> Pred
foreign import data Or :: Pred -> Pred -> Pred
data PredProxy (pred :: Pred) = PredProxy

class IsConditions (cs :: # Type {- PredProxy -})
instance isConditionsRL ::
  ( RL.RowToList cs csRL
  , IsConditionsRL csRL
  )
  => IsConditions cs
class IsConditionsRL (csRL :: RowList)
instance isConditionsRLNil :: IsConditionsRL RL.Nil
instance isConditionsRLCons ::
  IsConditionsRL restRL
  => IsConditionsRL (RL.Cons label (PredProxy pred) restRL)

foreign import kind Node
foreign import data Node :: Node
foreign import kind Branch
foreign import data Branch :: # Type -> Branch
foreign import kind Field
foreign import data FieldNode :: Node -> Field
foreign import data FieldBranch :: Branch -> Field
data FieldProxy (field :: Field) = FieldProxy

foreign import kind Filter
foreign import data FilterNode :: Pred -> Node -> Filter
foreign import data FilterBranch :: # Type {- PredProxy -} -> # Type {- FieldProxy -} -> Filter

foreign import kind Optic
foreign import data Filtered :: Filter -> Optic
foreign import data Indexed :: Symbol -> Field -> Optic

class IsPaths (paths :: # Type)
class IsPathsRL (pathsRL :: RowList)
instance isPathsRLNil :: IsPathsRL RL.Nil
instance isPahtsRLCons ::
  IsPathsRL restRL
  => IsPathsRL (RL.Cons label (FieldProxy field) restRL)

{- NOTE Foreign Key ( a -> edge -> b )

extra constraints
1. Nullable / Not-null
2. Non-unique / Unique

variants
F1. Nullable + Non-unique (Maybe, Array)

a -> b |   partial map | 1   -> 0/1 relation |       a -> Maybe b
a <- b |       not map | 0+  <-   1 relation | Array a <-       b

F2. Not-null + Non-unique (Identity, Array)

a -> b |           map | 1   ->   1 relation |       a ->       b
a <- b |       not map | 0+  <-   1 relation | Array a <-       b

F3. Nullable + Unique (Maybe, Maybe)

a -> b |   partial map | 1   -> 0/1 relation |       a -> Maybe b
a <- b |   partial map | 0/1 <-   1 relation | Maybe a <-       b

F4. Not-null + Unique (Identity, Maybe)

a -> b | injective map | 1   ->   1 relation |       a ->       b
a <- b |   partial map | 0/1 <-   1 relation | Maybe a <-       b

-}

{- NOTE Product Table (a, b)

a, b both Not-null

variants
P1. (Unique, Unique) (Maybe, Maybe) NOTE isomorphic to F3

a -> b | partial map | 1   -> 0/1 relation |       a -> Maybe b
a <- b | partial map | 0/1 <-   1 relation | Maybe a <-       b

P2. (Unique, Not-unique) (Maybe, Array) NOTE isomorphic to F1

a -> b | partial map | 1   -> 0/1 relation |       a -> Maybe b
a <- b |     not map | 0+  <-   1 relation | Array a <-       b

P3. (Not-unique, Not-unique) (Array, Array)

a -> b |     not map | 1   ->  0+ relation |       a -> Array b
a <- b |     not map | 0+  <-   1 relation | Array a <-       b


-}

{- NOTE dual-way Foreign Keys (a -> b, a <- b)

variants
1. (Nullable + Non-unique, Nullable + Non-unique) (Maybe, Maybe) NOTE F3, P1

a -> b | partial map | 1 -> 0/1 relation |       a -> Maybe b
a <- b | partial map | 0/1 <- 1 relation | Maybe a <-       b

2. (Nullable + Non-unique, Not-null + Non-unique) (Identity, Maybe) NOTE F4

a -> b |   partial map | 1 -> 0/1 relation |     a -> Maybe b
a <- b | injective map | 1 ->   1 relation |     a <-       b

3. (Not-null + Non-unique, Not-null + Non-unique) (Identity, Identity)

a -> b |   isomorphism | 1 ->   1 relation |     a ->       b
a <- b |   isomorphism | 1 ->   1 relation |     a <-       b


-}


{- NOTE

newtype Person = Person
  { name :: String
  , friends :: Array Person
  }

Subject -> edge -> Object

type

-}
