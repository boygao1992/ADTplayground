module Type.Data.FuncList where

foreign import kind FuncList
foreign import data Cons :: Type -> FuncList -> FuncList
foreign import data Nil :: Type -> FuncList

class FuncToFuncList f (fl :: FuncList) | f -> fl

instance funcToFuncListInductionStep ::
  ( FuncToFuncList b restFl
  ) => FuncToFuncList (a -> b) (Cons a restFl)
else instance funcToFuncListBaseCase ::
  FuncToFuncList a (Nil a)
