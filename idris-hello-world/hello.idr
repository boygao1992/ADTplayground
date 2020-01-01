module Hello

import Data.Fin
import Data.Vect

myPair : (Int, String)
myPair = (0, "")

anyVect : (n : Nat ** Vect n String)
anyVect = (3 ** ["a", "b", "c"])

data UType
  = UBoolean
  | UInt
  | UString
  | UFun UType UType

Show UType where
  show UBoolean = "UBoolean"
  show UInt = "UInt"
  show UString = "UString"
  show (UFun x y) = "UFun " <+> show x <+> " " <+> show y

interpUType : UType -> Type
interpUType UBoolean = Bool
interpUType UInt = Int
interpUType UString = String
interpUType (UFun x y) = interpUType x -> interpUType y

data ExprType : Type where
  BooleanF : Bool -> ExprType
  IntF : Int -> ExprType
  StringF : String -> ExprType

reflectExprType : ExprType -> UType
reflectExprType (BooleanF _) = UBoolean
reflectExprType (IntF _) = UInt
reflectExprType (StringF _) = UString

-- `idx`: position of the UType in the Vect
-- to get the third UType in the Vect, we need a term-level proof `Pop (Pop Stop)`
data HasType : (idx : Fin n) -> Vect n UType -> UType -> Type where
  Stop : {ts : Vect n UType} -> HasType FZ (t :: ts) t
  Pop : {us : Vect n UType} -> HasType k us t -> HasType (FS k) (u :: us) t

x : HasType (FS (FS FZ)) [UBoolean, UInt, UString] UString
x = Pop (Pop Stop)

hasType : Fin n -> Vect n UType -> UType
hasType FZ (t :: ts) = t
hasType (FS k) (_ :: ts) = hasType k ts

using (ts : Vect n UType)
  data Expr : Vect n UType -> UType -> Type where
    Var : HasType i ts x -> Expr ts x
    Val : (x : ExprType) -> Expr ts (reflectExprType x)
    Lam : Expr (x :: ts) y -> Expr ts (UFun x y)
    App : Expr ts (UFun x y) -> Expr ts x -> Expr ts y
    UnOp :
      (interpUType x -> interpUType y) ->
      Expr ts x -> Expr ts y
    BiOp :
      (interpUType x -> interpUType y -> interpUType z) ->
      Expr ts x -> Expr ts y -> Expr ts z
    If : Expr ts UBoolean -> Expr ts x -> Expr ts x -> Expr ts x

main : IO ()
main = putStrLn (show (hasType (FS(FS(FZ))) [UInt, UString, UBoolean]))
