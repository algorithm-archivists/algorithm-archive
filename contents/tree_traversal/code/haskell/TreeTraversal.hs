data Tree a = Node
  { node :: a,
    forest :: [Tree a]
  }
  deriving (Show)

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
    go ((Node x ts) : stack) = x : go (ts ++ stack)

bfs :: Tree a -> [a]
bfs (Node x ts) = x : go ts
  where
    go [] = []
    go ts = map node ts ++ go (concatMap forest ts)

createTree :: Int -> Int -> Tree Int
createTree 0 _ = Node 0 []
createTree numRow numChild = Node numRow children
  where
    children = map (createTree (numRow - 1)) $ replicate numChild numChild

main = do
  let testTree = createTree 2 3
      showNodes = unwords . map show
  putStrLn "[#]\nRecursive DFS:"
  putStrLn $ showNodes $ dfs testTree
  putStrLn "[#]\nRecursive Postorder DFS:"
  putStrLn $ showNodes $ dfsPostOrder testTree
  putStrLn "[#]\nStack-based DFS:"
  putStrLn $ showNodes $ dfsStack testTree
  putStrLn "[#]\nQueue-based BFS:"
  putStrLn $ showNodes $ bfs testTree
  putStrLn "[#]\nRecursive Inorder DFS for Binary Tree:"
  putStrLn $ showNodes $ dfsInOrder $ createTree 3 2
