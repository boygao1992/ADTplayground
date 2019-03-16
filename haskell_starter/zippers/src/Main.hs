{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wall #-}

module Main where

import qualified Control.Monad.ST as ST
import qualified Data.STRef as STRef

import qualified Data.List as List
-- import Control.Monad.State.Strict
import Control.Category ((<<<))
import Data.Maybe (mapMaybe)

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show)

deriving instance Eq a => Eq (Tree a)

freeTree :: Tree Char
freeTree =
  Node 'P'
  ( Node 'O'
    ( Node 'L'
      (Node 'N' Empty Empty)
      (Node 'T' Empty Empty)
    )
    ( Node 'Y'
      (Node 'S' Empty Empty)
      (Node 'A' Empty Empty)
    )
  )
  ( Node 'L'
    ( Node 'W'
      (Node 'C' Empty Empty)
      (Node 'R' Empty Empty)
    )
    ( Node 'A'
      (Node 'A' Empty Empty)
      (Node 'C' Empty Empty)
    )
  )

data Direction
  = L
  | R
  deriving (Show)

elemAt :: forall a. [Direction] -> Tree a -> Either String a
elemAt _ Empty = Left "Invalid Path"
elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = Right x

goLeft :: forall a. (Tree a, [Direction]) -> Either String (Tree a, [Direction])
goLeft (Node _ l _, dirs) = Right (l, L : dirs)
goLeft _ = Left "Invalid Move"

goRight :: forall a. (Tree a, [Direction]) -> Either String (Tree a, [Direction])
goRight (Node _ _ r, dirs) = Right (r, R : dirs)
goRight _ = Left "Invalid Move"

-- e.g.
-- goLeft =<< goRight =<< goLeft (freeTree, [])
-- = Right (Node 'S' Empty Empty, [L,R,L])

data Crumb a
  = LeftCrumb a (Tree a)
  | RightCrumb a (Tree a)
  deriving (Show)

deriving instance Eq a => Eq (Crumb a)

goLeft' :: forall a. (Tree a, [Crumb a]) -> Either String (Tree a, [Crumb a])
goLeft' (Node x l r, cs) = Right (l, (LeftCrumb x r) : cs)
goLeft' _ = Left "Invalid Move"

goRight' :: forall a. (Tree a, [Crumb a]) -> Either String (Tree a, [Crumb a])
goRight' (Node x l r, cs) = Right (r, (RightCrumb x l) : cs)
goRight' _ = Left "Invalid Move"

goUp' :: forall a. (Tree a, [Crumb a]) -> Either String (Tree a, [Crumb a])
goUp' (l, (LeftCrumb x r) : cs) = Right (Node x l r, cs)
goUp' (r, (RightCrumb x l) : cs) = Right (Node x l r, cs)
goUp' _ = Left "Invalid Move"

-- e.g.
-- (goUp' =<< goUp' =<< goRight' =<< goLeft' (freeTree, [])) == Right (freeTree, [])
-- = True

type TreeZipper a = (Tree a, [Crumb a])

modify :: forall a. (a -> a) -> TreeZipper a -> TreeZipper a
modify f (Node x l r, cs) = (Node (f x) l r, cs)
modify _ (Empty, cs) = (Empty, cs)

topMost :: forall a. TreeZipper a -> Either String (Tree a)
topMost (t, []) = Right t
topMost zipper = do
  upperZipper <- goUp' zipper
  topMost upperZipper

-- e.g.
-- topMost =<< fmap (modify (const 'Z')) . goLeft' =<< goRight' (freeTree, [])
-- = Right
--   ( Node 'P'
--     (...)
--     ( Node 'L'
--       ( Node 'Z'
--         ...
--       )
--       (...)
--     )
--   )

tree :: Tree Int
tree = Node 0
       ( Node 1
           Empty
           (Node 3 Empty Empty)
       )
       ( Node 2
           (Node 4 Empty Empty)
           (Node 5 Empty Empty)
       )

-- TODO BFS
-- normalize =
-- Node x l r -> State { result :: [x], queue :: [l, r] }
-- Empty -> State { result :: [], queue :: [] }
-- foldMap normalize queue :: State a

preOrderDFS :: forall a. Tree a -> [a]
preOrderDFS Empty = []
preOrderDFS (Node x l r) = [x] ++ preOrderDFS l ++ preOrderDFS r

inOrderDFS :: forall a. Tree a -> [a]
inOrderDFS Empty = []
inOrderDFS (Node x l r) = inOrderDFS l ++ [x] ++ inOrderDFS r

postOrderDFS :: forall a. Tree a -> [a]
postOrderDFS Empty = []
postOrderDFS (Node x l r) = postOrderDFS l ++ postOrderDFS r ++ [x]

-- NOTE CoRecursion
bfs :: forall a. Tree a -> [a]
bfs = go <<< pure
  where
    go :: [Tree a] -> [a]
    go [] = []
    go nodes = mapMaybe nodeValue nodes ++ concatMap nodeChildren nodes

    nodeValue :: Tree a -> Maybe a
    nodeValue Empty = Nothing
    nodeValue (Node x _ _) = Just x

    nodeChildren :: Tree a -> [a]
    nodeChildren Empty = []
    nodeChildren (Node _ l r) = mapMaybe nodeValue [l, r] ++ nodeChildren l ++ nodeChildren r


main :: IO ()
main = do
  putStrLn "hello world"

  let x = ST.runST
          ( do
              ref <- STRef.newSTRef (0 :: Integer)
              STRef.modifySTRef ref (+ 1)
              STRef.readSTRef ref
          )
  print x

  print tree

  print $ preOrderDFS tree
  print $ inOrderDFS tree
  print $ postOrderDFS tree
  print $ bfs tree

