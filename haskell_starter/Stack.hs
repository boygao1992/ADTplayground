module Stack where

import Prelude

data Stack a = Stack Int [a] deriving (Show)

-- constructors
empty :: Stack a
empty = Stack 0 []

fromList :: [a] -> Stack a
fromList xs = Stack (length xs) xs

-- operators
push :: a -> Stack a -> Stack a
push x (Stack n xs) = Stack (n+1) (x : xs)

pop :: Stack a -> Either String (Stack a, a)
pop (Stack _ []) = Left "Popping an empty stack."
pop (Stack n (x : xs)) = Right (Stack (n-1) xs, x)

peek :: Stack a -> Maybe a
peek (Stack _ []) = Nothing
peek (Stack _ (x : _)) = Just x

size :: Stack a -> Int
size (Stack n _) = n

-- predicates
isEmpty :: Stack a -> Bool
isEmpty (Stack _ []) = True
isEmpty _ = False
