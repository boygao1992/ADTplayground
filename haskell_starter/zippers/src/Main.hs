{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE
  BlockArguments
, DeriveDataTypeable
, DeriveFoldable
, DeriveFunctor
, DeriveGeneric
, DeriveTraversable
, ExplicitForAll
, FunctionalDependencies
, FlexibleInstances
, FlexibleContexts
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, NamedFieldPuns
, NoImplicitPrelude
, RankNTypes
, StandaloneDeriving
, ScopedTypeVariables
#-}

module Main where

import qualified Control.Monad.ST as ST
import qualified Data.STRef as STRef
import Control.Category ((<<<))
import Data.Maybe (mapMaybe)
import Prelude
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as Map

-- Clown and Joker

data Expr
  = Val Int
  | Add Expr Expr
 deriving (Eq, Ord, Show)

exprExample :: Expr
exprExample =
  ((Val 1) `Add` (Val 2)) `Add` (Val 3)

type ExprStack = [Expr -- NOTE suspended
                  `Either`
                  Int -- NOTE evaluated
                 ]

eval :: Expr -> Int
eval e = load e []

load :: Expr -> ExprStack -> Int -- NOTE destructure Expr
load (Val v) stk = unload v stk
load (Add e1 e2) stk = load e1 (Left e2 : stk)

unload :: Int -> ExprStack -> Int -- NOTE destructure ExprStack
unload v [] = v
unload v1 (Left e2 : stk) = load e2 (Right v1 : stk)
unload v2 (Right v1 : stk) = unload (v1 + v2) stk


-- XMonad
data XMonadStack a
  = XMonadEmpty
  | XMonadNode
      { focus :: a
      , left :: [a] -- clowns to the left
      , right :: [a] -- jokers to the right
      }

data XMonadWorkspace a = XMonadWorkspace
  { tag :: Int
  , stack :: XMonadStack a
  }

data XMonadStackSet a = XMonadStackSet
  { current :: XMonadWorkspace a -- NOTE force StackSet to be non-empty
  , prev :: [XMonadWorkspace a]
  , next :: [XMonadWorkspace a]
  }


-- Zipper

data ZList a = ZList [a] [a]
  deriving (Eq, Ord, Show, Functor)

zLeft :: ZList a -> Either String (ZList a)
zLeft (ZList (l : ls) rs) = Right $ ZList ls (l : rs)
zLeft _ = Left "Invalid move"

zRight :: ZList a -> Either String (ZList a)
zRight (ZList ls (r : rs)) = Right $ ZList (r : ls) rs
zRight _ = Left "Invalid move"

zSet :: a -> ZList a -> Either String (ZList a)
zSet x (ZList ls (_ : rs)) = Right $ ZList ls (x : rs)
zSet _ _ = Left "Invalid move"

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

nodeValue :: forall a. Tree a -> Maybe a
nodeValue Empty = Nothing
nodeValue (Node x _ _) = Just x

nodeChildren :: forall a. Tree a -> [a]
nodeChildren Empty = []
nodeChildren (Node _ l r) = mapMaybe nodeValue [l, r] ++ nodeChildren l ++ nodeChildren r

snoc :: forall a. [a] -> a -> [a]
xs `snoc` x = xs ++ [x]

insert :: forall a. Int -> a -> Map Int [a] -> Map Int [a]
insert n x dict =
  if (Map.member n dict)
  then Map.update (Just <<< (`snoc` x)) n dict
  else Map.insert n [x] dict

vertical :: forall a. Tree a -> Map Int [a]
vertical root = go 0 root Map.empty
  where
  go :: Int -> Tree a -> Map Int [a] -> Map Int [a]
  go _ Empty dict = dict
  go n (Node x l r) dict =
    let dict' = go (n - 1) l dict
        dict'' = insert n x dict'
        dict''' = go (n + 1) r dict''
    in
      dict'''

children :: forall a. (Int, Tree a) -> [(Int, Tree a)]
children (n, Empty) = [(n - 1, Empty), (n + 1, Empty)]
children (n, Node _ l r) = [(n - 1, l), (n + 1, r)]

branching :: forall a. [(Int, Tree a)] -> [(Int, Tree a)]
branching = concatMap children

reducer :: forall a. ([(Int, Tree a)], Map Int [a]) -> (Int, Tree a) -> ([(Int, Tree a)], Map Int [a])
reducer (xs, dict) (_, Empty) = (xs, dict)
reducer (xs, dict) (n, Node x l r) = (xs ++ [(n - 1, l), (n + 1, r)], insert n x dict)

level :: forall a. [(Int, Tree a)] -> Map Int [a] -> ([(Int, Tree a)], Map Int [a])
level xs dict = foldl' reducer ([], dict) xs

vertical' :: forall a. Tree a -> Map Int [a]
vertical' root = go [(0, root)] Map.empty
  where
    go :: [(Int, Tree a)] -> Map Int [a] -> Map Int [a]
    go xs dict =
      let (xs', dict') = level xs dict
      in
        if length xs' == 0
        then dict'
        else go xs' dict'

{-
           0
     -1          1
  -2    0     0    +2
-3 -1 -1 +1 -1 +1 +1 +3

       0
   0       1
 0   1   2   3
0 1 2 3 4 5 6 7

-}
-- | Matrix
type Vector = [Double]
type Matrix = [Vector]

matrix0 :: Matrix
matrix0 = [ [1.0, 2.0]
          , [3.0, 4.0]
          ]

matrix1 :: Matrix
matrix1 = [ [1.0, 0.0]
          , [0.0, 1.0]
          ]

dot :: Vector -> Vector -> Maybe Double
dot [] [] = Just 0.0
dot [] _ = Nothing
dot _ [] = Nothing
dot (x: xs) (y: ys) = (x * y +) <$> dot xs ys

fromRowVec :: Vector -> Matrix
fromRowVec = pure

fromColVec :: Vector -> Matrix
fromColVec [] = []
fromColVec (x:xs) = [x] : fromColVec xs

transp :: Matrix -> Maybe Matrix
transp [] = Just []
transp (v : vs) = do
  let m1 = fromColVec v
  m2 <- transp vs
  if m2 == []
    then pure m1
    else pure $ zipWith (++) m1 m2

matrixMul :: Matrix -> Matrix -> Maybe Double
matrixMul [] [] = Just 0.0
matrixMul [] _ = Nothing
matrixMul _ [] = Nothing
matrixMul (x:xs) (y:ys) = pure (+) <*> x `dot` y <*> matrixMul xs ys

-- | Mulitplicative
class Semiring a where
  zero :: a
  add :: a -> a -> a
  one :: a
  mul :: a -> a -> a

instance Semiring Double where
  zero = 0.0
  one = 1.0
  add = (+)
  mul = (*)

class Semiring a => Ring a where
  neg :: a -> a

newtype Multiplicative a = Multiplicative a
instance Semiring a => Semigroup (Multiplicative a) where
  (Multiplicative x) <> (Multiplicative y) = Multiplicative $ x `mul` y
instance Semiring a => Monoid (Multiplicative a) where
  mempty = Multiplicative one

-- | Heap
data Heap a
  = EmptyHP
  | HP a Int (Heap a) (Heap a)
  deriving (Show)

emptyHeap :: forall a. Heap a
emptyHeap = EmptyHP

singletonHeap :: forall a. a -> Heap a
singletonHeap x = HP x 1 EmptyHP EmptyHP

nullHeap :: forall a. Heap a -> Bool
nullHeap EmptyHP = True
nullHeap _ = False

findHeap :: forall a. Heap a -> Maybe a
findHeap (HP x _ _ _ ) = Just x
findHeap _ = Nothing

rankHeap :: forall a. Heap a -> Int
rankHeap EmptyHP = 0
rankHeap (HP _ r _ _) = r

makeHP :: forall a. a -> Heap a -> Heap a -> Heap a
makeHP x l r =
  let r1 = rankHeap l
      r2 = rankHeap r
  in
    if r1 >= r2
    then HP x (r2 + 1) l r
    else HP x (r1 + 1) r l

mergeHeap :: forall a. Ord a => Heap a -> Heap a -> Heap a
mergeHeap l EmptyHP = l
mergeHeap EmptyHP r = r
mergeHeap l@(HP x _ ll lr) r@(HP y _ rl rr)
  | x <= y = makeHP x ll (mergeHeap lr r)
  | otherwise = makeHP y rl (mergeHeap l rr)

insertHeap :: forall a. Ord a => Heap a -> a -> Heap a
insertHeap h x = mergeHeap (singletonHeap x) h

popHeap :: forall a. Ord a => Heap a -> Maybe (a, Heap a)
popHeap EmptyHP = Nothing
popHeap (HP x _ l r) = Just $ (x, mergeHeap l r)

fromFoldable :: forall a f. (Ord a, Foldable f) => f a -> Heap a
fromFoldable = foldl' insertHeap emptyHeap -- NOTE strict

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

  print $ matrix0
  print $ transp matrix0

  print $ matrixMul matrix0 matrix1

  print $ vertical tree

  print $ vertical' tree
