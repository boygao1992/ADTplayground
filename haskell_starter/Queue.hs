module Queue where

import Prelude

data Queue a = Queue { _size :: Int, _in :: [a], _out :: [a] }

-- constructors
empty :: Queue a
empty = Queue 0 [] []

fromList :: [a] -> Queue a
fromList xs = Queue (length xs) xs []

-- operators
push :: a -> Queue a -> Queue a
push x (Queue n xs ys) = Queue (n+1) (x : xs) ys

pop :: Queue a -> Either String (Queue a, a)
pop q =
  case (peek q) of
    Right (Queue n xs (y : ys), _) -> Right (Queue (n-1) xs ys, y)
    _ -> Left "Popping an empty queue."

peek :: Queue a -> Either (Queue a) (Queue a, a)
peek q@(Queue _ [] []) = Left q
peek (Queue n xs []) = peek (Queue n [] (reverse xs)) -- O(n)
peek q@(Queue _ _ (y : _)) = Right (q, y)

size :: Queue a -> Int
size (Queue n _ _) = n

-- predicate
isEmpty :: Queue a -> Bool
isEmpty (Queue _ [] []) = True
isEmpty _ = False
