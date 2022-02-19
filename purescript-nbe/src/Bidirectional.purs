-- | Checking Dependent Types with Normalization by Evaluation: A Tutorial
-- | Section 4
module Bidirectional
  ( Context
  , Env(..)
  , Expr(..)
  , Message(..)
  , Name(..)
  , Ty(..)
  , check
  , synth
  , test
  ) where

import Prelude
import Data.Array as Data.Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic as Data.Show.Generic

newtype Name
  = Name String

derive instance eqName :: Eq Name

derive instance genericName :: Generic Name _

instance showName :: Show Name where
  show x = Data.Show.Generic.genericShow x

newtype Message
  = Message String

derive instance genericMessage :: Generic Message _

instance showMessage :: Show Message where
  show x = Data.Show.Generic.genericShow x

failure :: forall a. String -> Either Message a
failure message = Left (Message message)

newtype Env value
  = Env (Array { name :: Name, value :: value })

derive instance genericEnv :: Generic (Env value) _

instance showEnv :: Show value => Show (Env value) where
  show x = Data.Show.Generic.genericShow x

initEnv :: forall value. Env value
initEnv = Env []

lookupVar :: forall value. Env value -> Name -> Either Message value
lookupVar (Env xs) x = case Data.Array.uncons xs of
  Nothing -> failure ("Not found: " <> show x)
  Just { head: { name: y, value }, tail }
    | y == x -> Right value
    | otherwise -> lookupVar (Env tail) x

extend :: forall value. Env value -> Name -> value -> Env value
extend (Env xs) name value = Env (Data.Array.cons { name, value } xs)

type Context
  = Env Ty

initContext :: Context
initContext = initEnv

data Expr
  = Var Name
  | Lambda Name Expr
  | App Expr Expr
  | Zero
  | Succ Expr
  | Rec Ty Expr Expr Expr
  | Ann Expr Ty

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = Data.Show.Generic.genericShow x

data Ty
  = TNat
  | TArr Ty Ty

derive instance eqTy :: Eq Ty

derive instance genericTy :: Generic Ty _

instance showTy :: Show Ty where
  show x = Data.Show.Generic.genericShow x

synth :: Context -> Expr -> Either Message Ty
synth ctx = case _ of
  Var x -> lookupVar ctx x
  App e1 e2 -> do
    tyE1 <- synth ctx e1
    case tyE1 of
      TArr tyA tyB -> do
        check ctx e2 tyA
        pure tyB
      _ -> failure ("Not a function type: " <> show tyE1)
  Rec ty n b s -> do
    tyN <- synth ctx n
    case tyN of
      TNat -> do
        check ctx b ty
        check ctx s (TArr TNat (TArr ty ty))
        pure ty
      _ -> failure ("Not a type Nat: " <> show tyN)
  Ann e ty -> do
    check ctx e ty
    pure ty
  other -> failure ("Can't find a type for " <> show other <> ". Try adding a type annotation.")

check :: Context -> Expr -> Ty -> Either Message Unit
check ctx expr ty = case expr of
  Lambda x scope -> case ty of
    TArr tyA tyB -> check (extend ctx x tyA) scope tyB
    _ -> failure ("Lambda requires a function type, but got " <> show ty)
  Zero -> case ty of
    TNat -> pure unit
    _ -> failure ("Zero should be a Nat, but was used where a " <> show ty <> " was expected.")
  Succ n -> case ty of
    TNat -> check ctx n TNat
    _ -> failure ("Succ should be a Nat, but was used where a " <> show ty <> " was expected.")
  _ -> do -- NOTE Mode change
    ty2 <- synth ctx expr
    if ty2 == ty then
      pure unit
    else
      failure ("Expected " <> show ty <> " but got " <> show ty2)

addDefs :: Context -> Array { name :: Name, def :: Expr } -> Either Message Context
addDefs ctx xs = case Data.Array.uncons xs of
  Nothing -> pure ctx
  Just { head: { name: x, def: e }, tail } -> do
    ty <- synth ctx e
    addDefs (extend ctx x ty) tail

test ::
  Either Message
    { example1 :: { expr :: Expr, type :: Ty }
    , example2 :: { expr :: Expr, type :: Ty }
    }
test = do
  ctx <-
    addDefs initContext
      [ { name: Name "2", def: two }
      , { name: Name "3", def: three }
      , { name: Name "+", def: plus }
      ]
  let
    expr1 :: Expr
    expr1 =
      App
        (Var (Name "+"))
        (Var (Name "3"))

    expr2 :: Expr
    expr2 =
      App
        ( App
            (Var (Name "+"))
            (Var (Name "3"))
        )
        (Var (Name "2"))
  ty1 <- synth ctx expr1
  ty2 <- synth ctx expr2
  pure
    { example1: { expr: expr1, type: ty1 }
    , example2: { expr: expr2, type: ty2 }
    }

two :: Expr
two =
  Ann
    (Succ (Succ Zero))
    TNat

three :: Expr
three =
  Ann
    (Succ (Succ (Succ Zero)))
    TNat

plus :: Expr
plus =
  Ann
    ( Lambda (Name "n")
        ( Lambda (Name "k")
            ( Rec TNat (Var (Name "n"))
                (Var (Name "k"))
                ( Lambda (Name "_pred")
                    ( Lambda (Name "acc")
                        (Succ (Var (Name "acc")))
                    )
                )
            )
        )
    )
    (TArr TNat (TArr TNat TNat))
