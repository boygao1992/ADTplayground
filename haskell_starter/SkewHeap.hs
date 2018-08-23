module SkewHeap where

import Prelude

data SkewHeap a
  = Empty
  | Node a (SkewHeap a) (SkewHeap a) deriving (Show)

singleton :: Ord a => a -> SkewHeap a
singleton x = Node x Empty Empty

(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
Empty +++ heap2 = heap2
heap1 +++ Empty = heap1
heap1@(Node x1 l1 r1) +++ heap2@(Node x2 l2 r2)
  | x1 <= x2 = Node x1 (heap2 +++ r1) l1
  | otherwise = Node x2 (heap1 +++ r2) l2

extractMin :: Ord a => SkewHeap a -> Maybe (SkewHeap a, a)
extractMin Empty = Nothing
extractMin (Node x l r) = Just (l +++ r, x)

fromList :: Ord a => [a] -> SkewHeap a
fromList [] = Empty
fromList (x : xs) = singleton x +++ fromList xs
