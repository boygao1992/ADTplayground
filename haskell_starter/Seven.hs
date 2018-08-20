module Seven where

import Prelude

{- | given the results of preorder traversal and inorder traversal of a binary tree
   | reconstruct the tree
   | note: no repeated values
 -}

data BTree
  = Branch Int BTree BTree
  | Node Int
  | Empty
  deriving (Show)

onLeft :: [Int] -> Int -> Int -> Bool
onLeft [] _ _ = True
onLeft (y : ys) x target
  | x == y = True
  | target == y = False
  | otherwise = onLeft ys x target

-- hash :: [Int] -> Dict Int Int
insert :: (Int -> Int -> Bool) -> Int -> BTree -> BTree
insert _ i Empty = Node i
insert l i (Node x)
  | l i x = Branch x (Node i) Empty
  | otherwise = Branch x Empty (Node i)
insert l i (Branch x left right)
  | l i x = Branch x (insert l i left) right
  | otherwise = Branch x left (insert l i right)

preorder :: [Int]
preorder = [3,9,20,15,7]

inorder :: [Int]
inorder = [9,3,15,20,7]

btree :: BTree
btree = foldl (\acc x -> insert (onLeft inorder) x acc) Empty preorder
