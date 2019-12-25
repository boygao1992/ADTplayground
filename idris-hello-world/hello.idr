module Main

import Data.Vect

myPair : (Int, String)
myPair = (0, "")

anyVect : (n : Nat ** Vect n String)
anyVect = (3 ** ["a", "b", "c"])

data Expr : Type -> Type where
  Val : a -> Expr a
  Lookup : {a : Type} -> String -> Expr a
  If : Expr Bool -> Expr a -> Expr a -> Expr a

main : IO ()
main = putStrLn "Hello World"
