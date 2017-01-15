data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Problem 55
-- cbalTree
-- Construct completely balanced binary trees
merge :: [a] -> [a] -> [a]
merge (x:xs) ys = x: merge ys xs


trees = [Empty]:merge oddTrees evenTrees
oddTrees = [[Branch 'x' l r | l <- a, r <- a] | a <- trees]
evenTrees =  [concat [[Branch 'x' l r, Branch 'x' r l] | l <- left, r <- right] | (left, right) <- (zip trees (tail trees))]

cbalTree :: Int -> [Tree Char]
cbalTree = (trees !!)


-- Problem 56
-- symmetric
-- Symmetric binary trees
-- Write a function to check if a tree is symmetric

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch _ l1 r1) (Branch _ l2 r2) = and [mirror l1 r2, mirror r1 l2]

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r


-- Problem 57
-- construct
-- Construct a binary search tree from a list

add :: Ord a => Tree a -> a -> Tree a
add Empty a = Branch a Empty Empty
add t@(Branch v l r) a = case compare a v of
                        LT -> Branch v (add l a) r
                        GT -> Branch v l (add r a)
                        EQ -> t

construct :: Ord a => [a] -> Tree a
construct = foldl add Empty


-- Problem 58
-- symCbalTrees
-- Construct all symmetric, completely balanced trees with a given number of nodes
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree


-- Problem 59
-- hbalTree
-- Construct height balanced binary trees of given element and macimum height
hbalTree :: a -> Int -> [Tree a]
hbalTree x n = hbaltrees !! n where
                hbaltrees = [Empty]:[Branch x Empty Empty] : 
                            zipWith combine hbaltrees (tail hbaltrees)
                combine ts shortts = [Branch x l r | 
                            (ls, rs) <- [(ts, shortts), (ts, ts), (shortts, ts)],
                            l <- ls, r <- rs]

