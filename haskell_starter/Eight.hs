module Eight where

import Prelude

data BTree = BTree { _id :: Int, _parent :: Maybe BTree, _children :: (Maybe BTree, Maybe BTree)}
  -- if parent == Nothing then Root
  -- if children == (Nothing, Nothing) then Leaf

data State
  = Up BTree
  | Down

root :: BTree
root = BTree 1 Nothing (Just branch1, Just branch2)

branch1 :: BTree
branch1 = BTree 2 (Just root) (Just leaf3, Just leaf1)

branch2 :: BTree
branch2 = BTree 3 (Just root) (Just leaf2, Nothing)

leaf1 :: BTree
leaf1 = BTree 4 (Just branch1) (Nothing, Nothing)

leaf2 :: BTree
leaf2 = BTree 5 (Just branch2) (Nothing, Nothing)

leaf3 :: BTree
leaf3 = BTree 6 (Just branch1) (Nothing, Nothing)

start :: BTree
start = leaf3

search :: State -> BTree -> Either String BTree
search (Up from) current@(BTree _ p (Just l, Just r))
  | _id from == _id l = Right current
  | _id from == _id r = case p of
      Just pa -> search (Up current) pa
      Nothing -> Left "Done traversing already"
  | otherwise = Left "Err in Tree Construction"
search (Up from) current@(BTree _ _ (Just l, Nothing))
  | _id from == _id l = Right current
  | otherwise = Left "Err in Tree Construction"
search (Up from) current@(BTree _ p (Nothing, Just r))
  | _id from == _id r = case p of
      Just pa -> search (Up current) pa
      Nothing -> Right current
  | otherwise = Left "Err in Tree Construction"
search (Up from) current@(BTree id2 p (Nothing, Nothing))
  | _id from == id2 = case p of
      Just pa -> search (Up current) pa
      Nothing -> Right current
  | otherwise = Left "Err in Tree Construction"
search Down current@(BTree _ _ c) =
  case c of
    (Just l, _) -> search Down l
    (_, Just r) -> search Down r
    (Nothing, Nothing) -> Right current

state :: State
state = case start of
  BTree _ _ (_, Just _) -> Down
  _ -> Up start

result :: Either String BTree
result = search state start

showResult :: String
showResult =  either id (show . _id) result
