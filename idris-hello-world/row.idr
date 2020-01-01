module Main

import Data.HVect
import Data.Vect.Views

-- Data.Vect

sortBy : (a -> a -> Ordering) -> Vect n a -> Vect n a
sortBy f xs' = go (Data.Vect.Views.splitRec xs')
  where
  go : {xs : Vect n a} -> SplitRec xs -> Vect n a
  go {xs = []} SplitRecNil = []
  go {xs = [x]} SplitRecOne = [x]
  go (SplitRecPair lrec rrec) = Data.Vect.mergeBy f (go lrec) (go rrec)

-- Record

removeLabel : Vect n (String, a) -> Vect n a
removeLabel = map snd

data Record : Vect n (String, Type) -> Type where
  RecordCtor : HVect (removeLabel ls) -> Record ls

testRecord1 : Record [ ("age", Int), ("name", String) ]
testRecord1 = RecordCtor ( 1 :: "wenbo" :: Nil )

data Key : String -> Vect n (String, a) -> Type where
  KeyHere : Key k ((k, _) :: xs)
  KeyThere : (later : Key k xs) -> Key k (x :: xs)

getIndex : (key : String) -> (vec :Vect n (String, Type)) -> {auto prf :Key key vec} -> Fin n
getIndex k ((k, t) :: xs) {prf = KeyHere} = FZ
getIndex k (_ :: xs) {prf = KeyThere p} = FS (getIndex k xs {prf = p})

testGetIndex : Fin 2
testGetIndex = getIndex "x" (fromList [("y", Int), ("x", String)])

get :
  (k : String) ->
  Record row ->
  {auto prf : Key k row} ->
  Data.Vect.index (getIndex k row {prf}) (removeLabel row)
get {row} k (RecordCtor xs) {prf} = Data.HVect.index (getIndex k row {prf}) xs

testGet : String
testGet = get "name" testRecord1

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
