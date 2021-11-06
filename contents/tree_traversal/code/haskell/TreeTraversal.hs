data Tree a = Node
  { node :: a
  , forest :: [Tree a]
  } deriving (Show)

dfs :: Tree a -> [a]
dfs (Node x ts) = x : concatMap dfs ts

dfsPostOrder :: Tree a -> [a]
dfsPostOrder (Node x ts) = concatMap dfsPostOrder ts ++ [x]

dfsInOrder :: Tree a -> [a] -- For binary trees only
dfsInOrder (Node x []) = [x]
dfsInOrder (Node x [l]) = dfsInOrder l ++ [x] -- Single branch assumed to be left
dfsInOrder (Node x [l, r]) = dfsInOrder l ++ [x] ++ dfsInOrder r
dfsInOrder _ = error "Not a binary tree"

dfsStack :: Tree a -> [a]
dfsStack t = go [t]
  where
    go [] = []
    go ((Node x ts):stack) = x : go (ts ++ stack)

bfs :: Tree a -> [a]
bfs (Node x ts) = x : go ts
  where
    go [] = []
    go ts = map node ts ++ go (concatMap forest ts)

toBin :: Tree a -> Tree a
toBin (Node x ts) = Node x (map toBin $ take 2 ts)

main = do
  print $ dfs testTree
  print $ dfsPostOrder testTree
  print $ dfsInOrder $ toBin testTree
  print $ dfsStack testTree
  print $ bfs testTree

testTree :: Tree Int
testTree =
  Node
    1
    [ Node 2 [Node 3 [], Node 4 [Node 5 []]]
    , Node
        6
        [ Node 7 []
        , Node 8 [Node 9 [Node 10 [Node 11 []], Node 12 []]]
        , Node 13 [Node 14 []]
        ]
    , Node 15 []
    ]
