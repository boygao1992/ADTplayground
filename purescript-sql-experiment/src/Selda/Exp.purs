module Selda.Exp where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Exists1 (Exists, flippedRunExists, mkExists, runExists)
import Data.Exists2 (Exists2, flippedRunExists2, mkExists2)
import Data.Maybe (Maybe)
import Selda.SqlType (Lit, SqlTypeRep)
import Selda.Types (ColName)

type SomeExp sql = Exists (SomeF sql)
newtype SomeF sql a = SomeF (Exp sql a)
mkSomeExp :: forall sql a. Exp sql a -> SomeExp sql
mkSomeExp = mkExists <<< SomeF
runSomeExp :: forall sql r. SomeExp sql -> (forall a. Exp sql a -> r) -> r
runSomeExp someExp f = runExists (\(SomeF exp) -> f exp) someExp

-- | A type-erased column, which may also be renamed.
--   Only for internal use.
data SomeCol sql
  = Some (SomeExp sql)
  | Named ColName (SomeExp sql)
-- Some  :: !(Exp sql a) -> SomeCol sql
some :: forall sql a. Exp sql a -> SomeCol sql
some = Some <<< mkSomeExp
-- Named :: !ColName -> !(Exp sql a) -> SomeCol sql
named :: forall sql a. ColName -> Exp sql a -> SomeCol sql
named cn eA = Named cn (mkSomeExp eA)

data UntypedCol sql
  = Untyped (SomeExp sql)
-- Untyped :: !(Exp sql a) -> UntypedCol sql
untyped :: forall sql a. Exp sql a -> UntypedCol sql
untyped = Untyped <<< mkSomeExp

-- | Turn a renamed column back into a regular one.
--   If the column was renamed, it will be represented by a literal column,
--   and not its original expression.
hideRenaming :: forall sql. SomeCol sql -> UntypedCol sql
hideRenaming (Named cn _) = untyped $ col cn
hideRenaming (Some someF) = Untyped someF

-- | Underlying column expression type, parameterised over the type of
--   SQL queries.
data Exp sql a
  = Col ColName
  | Lit (Lit a)
  | BinOp (Exists2 (BinOpF2 sql a))
  | UnOp (Exists (UnOpF sql a))
  | NulOp (NulOp a)
  | Fun2 (Exists2 (Fun2F2 sql))
  | If (Exp sql Boolean) (Exp sql a) (Exp sql a)
  | Cast SqlTypeRep (SomeExp sql)
  | AggrEx String (SomeExp sql)
  | InList (Exists (InListF sql)) -- NOTE (a ~ Boolean)
  | InQuery (SomeExp sql) sql -- NOTE (a ~ Boolean)
data BinOpF2 sql c a b = BinOpF2 (BinOp a b c) (Exp sql a) (Exp sql b)
data UnOpF sql b a = UnOpF (UnOp a b) (Exp sql a)
data Fun2F2 sql a b = Fun2F2 String (Exp sql a)  (Exp sql b)
data InListF sql a = InListF (Exp sql a) (Array (Exp sql a))
-- Col     :: !ColName -> Exp sql a
col = Col :: forall sql a. ColName -> Exp sql a
-- Lit     :: !(Lit a) -> Exp sql a
lit = Lit :: forall sql a. Lit a -> Exp sql a
-- BinOp   :: !(BinOp a b c) -> !(Exp sql a) -> !(Exp sql b) -> Exp sql c
binOp :: forall sql a b c. BinOp a b c -> Exp sql a -> Exp sql b -> Exp sql c
binOp bop eA eB = BinOp $ mkExists2 $ BinOpF2 bop eA eB
-- UnOp    :: !(UnOp a b) -> !(Exp sql a) -> Exp sql b
unOp :: forall sql a b. UnOp a b -> Exp sql a -> Exp sql b
unOp uop eA = UnOp $ mkExists $ UnOpF uop eA
-- NulOp   :: !(NulOp a) -> Exp sql a
nulOp = NulOp :: forall sql a. NulOp a -> Exp sql a
-- Fun2    :: !Text -> !(Exp sql a) -> !(Exp sql b) -> Exp sql c
fun2 :: forall sql a b c. String -> Exp sql a -> Exp sql b -> Exp sql c
fun2 label eA eB = Fun2 $ mkExists2 $ Fun2F2 label eA eB
-- If      :: !(Exp sql Bool) -> !(Exp sql a) -> !(Exp sql a) -> Exp sql a
if_ = If :: forall sql a. Exp sql Boolean -> Exp sql a -> Exp sql a -> Exp sql a
-- Cast    :: !SqlTypeRep -> !(Exp sql a) -> Exp sql b
cast :: forall sql a b. SqlTypeRep -> Exp sql a -> Exp sql b
cast rep eA = Cast rep (mkSomeExp eA)
-- AggrEx  :: !Text -> !(Exp sql a) -> Exp sql b
aggrEx :: forall sql a b. String -> Exp sql a -> Exp sql b
aggrEx label eA = AggrEx label (mkSomeExp eA)
-- InList  :: !(Exp sql a) -> ![Exp sql a] -> Exp sql Bool
inList :: forall sql a. Exp sql a -> Array (Exp sql a) -> Exp sql Boolean
inList eA eAs = InList $ mkExists $ InListF eA eAs
-- InQuery :: !(Exp sql a) -> !sql -> Exp sql Bool
inQuery :: forall sql a. Exp sql a -> sql -> Exp sql Boolean
inQuery eA sql = InQuery (mkSomeExp eA) sql

data NulOp a
  = Fun0 String
-- Fun0 :: !Text -> NulOp a
fun0 = Fun0 :: forall a. String -> NulOp a

data UnOp a b
  = Abs
  | Not
  | Neg
  | Sgn
  | IsNull
  | Fun String
-- Abs    :: UnOp a a
abs = Abs :: forall a. UnOp a a
-- Not    :: UnOp Bool Bool
not = Not :: UnOp Boolean Boolean
-- Neg    :: UnOp a a
neg = Neg :: forall a. UnOp a a
-- Sgn    :: UnOp a a
sgn = Sgn :: forall a. UnOp a a
-- IsNull :: UnOp (Maybe a) Bool
isnull = IsNull :: forall a. UnOp (Maybe a) Boolean
-- Fun    :: !Text -> UnOp a b
fun = Fun :: forall a b. String -> UnOp a b

data BinOp a b c
  = Gt
  | Lt
  | Gte
  | Lte
  | Eq
  | Neq
  | And
  | Or
  | Add
  | Sub
  | Mul
  | Div
  | Like
  | CustomOp String
-- Gt   :: BinOp a a Bool
gt = Gt :: forall a. BinOp a a Boolean
-- Lt   :: BinOp a a Bool
lt = Lt :: forall a. BinOp a a Boolean
-- Gte  :: BinOp a a Bool
gte = Gte :: forall a. BinOp a a Boolean
-- Lte  :: BinOp a a Bool
lte = Lte :: forall a. BinOp a a Boolean
-- Eq   :: BinOp a a Bool
eq = Eq :: forall a. BinOp a a Boolean
-- Neq  :: BinOp a a Bool
neq = Neq :: forall a. BinOp a a Boolean
-- And  :: BinOp Bool Bool Bool
and = And :: BinOp Boolean Boolean Boolean
-- Or   :: BinOp Bool Bool Bool
or = Or :: BinOp Boolean Boolean Boolean
-- Add  :: BinOp a a a
add = Add :: forall a. BinOp a a a
-- Sub  :: BinOp a a a
sub = Sub :: forall a. BinOp a a a
-- Mul  :: BinOp a a a
mul = Mul :: forall a. BinOp a a a
-- Div  :: BinOp a a a
div = Div :: forall a. BinOp a a a
-- Like :: BinOp Text Text Bool
like = Like :: BinOp String String Boolean
-- CustomOp :: !Text -> BinOp a b c
customOp = CustomOp :: forall a b c. String -> BinOp a b c

class Names a where
  -- | Get all column names used in the given expression.
  allNamesIn :: a -> Array ColName

instance namesArray :: Names a => Names (Array a) where
  allNamesIn = Array.foldMap allNamesIn

instance namesExp :: Names sql => Names (Exp sql a) where
  allNamesIn (Col n)          = [n]
  allNamesIn (Lit _)          = []
  allNamesIn (BinOp binOpF2)  = flippedRunExists2 binOpF2 \(BinOpF2 _ a b) ->
    allNamesIn a <> allNamesIn b
  allNamesIn (UnOp unOpF)     = flippedRunExists unOpF \(UnOpF _ a) -> allNamesIn a
  allNamesIn (NulOp _)        = []
  allNamesIn (Fun2 fun2F2)    = flippedRunExists2 fun2F2 \(Fun2F2 _ a b) ->
    allNamesIn a <> allNamesIn b
  allNamesIn (If a b c)       = allNamesIn a <> allNamesIn b <> allNamesIn c
  allNamesIn (Cast _ x)       = runSomeExp x allNamesIn
  allNamesIn (AggrEx _ x)     = runSomeExp x allNamesIn
  allNamesIn (InList inListF) = flippedRunExists inListF \(InListF x xs) ->
    Array.foldMap allNamesIn (x:xs)
  allNamesIn (InQuery x q)    = runSomeExp x allNamesIn <> allNamesIn q

instance namesSomeCol :: Names sql => Names (SomeCol sql) where
  allNamesIn (Some c)    = runSomeExp c allNamesIn
  allNamesIn (Named n c) = n : runSomeExp c allNamesIn

instance namesUntypedCol :: Names sql => Names (UntypedCol sql) where
  allNamesIn (Untyped c) = runSomeExp c allNamesIn

