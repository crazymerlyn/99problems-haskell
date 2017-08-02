data Tree a = Node a [Tree a] deriving (Show, Eq)

-- Problem 70C
-- nnodes
-- Count the nodes of a multiway tree
nnodes :: Tree a -> Int
nnodes (Node _ children) = sum (map nnodes children) + 1


-- Problem 70
-- treeToString, stringToTree
-- Tree contruction from a node string
treeToString :: Tree Char -> [Char]
treeToString (Node c children) = c : concatMap treeToString children ++ "^"

stringToTree :: [Char] -> Tree Char
stringToTree (x:xs) = Node x (fst (help xs)) where
                    help ('^':rs) = ([], rs)
                    help (c:rest) = (thisNode:restNodes, left) where
                        (children, remaining) = help rest
                        thisNode = Node c children
                        (restNodes, left) = help remaining


-- Problem 71
-- ipl
-- Determine the internal path length of a tree
ipl :: Tree a -> Int
ipl (Node _ xs) = sum [pathLength x 1 | x <- xs] where
                    pathLength (Node _ rs) n = n + sum [pathLength r (n+1) | r <- rs]

