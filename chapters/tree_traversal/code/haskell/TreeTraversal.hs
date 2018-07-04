data Tree a = Node { node :: a
                   , forest :: [Tree a]
                   } deriving (Show)

dfs :: Tree a -> [a]
dfs (Node x ts) = x : concatMap dfs ts

dfsPostOrder :: Tree a -> [a]
dfsPostOrder (Node x ts) = concatMap dfsPostOrder ts ++ [x]

dfsInOrder :: Tree a -> [a] -- For binary trees only
dfsInOrder (Node x [])     = [x]
dfsInOrder (Node x [l])    = dfsInOrder l ++ [x] -- Single branch assumed to be left
dfsInOrder (Node x [l, r]) = dfsInOrder l ++ [x] ++ dfsInOrder r
dfsInOrder _               = error "Not a binary tree"

dfsStack :: Tree a -> [a]
dfsStack (Node x ts) = x : concatMap dfsStack ts

bfs :: Tree a -> [a]
bfs (Node x ts) = x : go ts
  where go [] = []
        go ts = map node ts ++ go (concatMap forest ts)


createTree :: Int -> Int -> Tree Int
createTree 0 _ = Node 0 []
createTree n c = Node n children
  where children = replicate c (createTree (n-1) c)


root = createTree 2 3
rootBinary = createTree 3 2
main = do
  print $ dfs root
  print $ dfsPostOrder root
  print $ dfsStack root
  print $ bfs root
  print $ dfsInOrder rootBinary
