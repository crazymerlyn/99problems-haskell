data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Problem 61
-- countLeaves
-- Count the leaves of a binary tree
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r


-- Problem 61A
-- leaves
-- Collect the leaves of a binary tree in a list
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r


-- Problem 62
-- internals
-- Collect the internal nodes of a binary tree in a list
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x:(internals l ++ internals r)


-- Problem 62B
-- atLevel
-- Collect the nodes at a given level in a list
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch _ _ _) 0 = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch x l r) n = atLevel l (n-1) ++ atLevel r (n-1)


-- Problem 63
-- completeBinaryTree
-- Construct a complete binary tree
completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree 1 = Branch 'x' Empty Empty
completeBinaryTree n = Branch 'x' l r where
                        powersOfTwo = 1:map (2*) powersOfTwo
                        nextPowerOfTwo x = head $ dropWhile (<= x) powersOfTwo
                        high = nextPowerOfTwo (n+1) - 1
                        low  = div (nextPowerOfTwo (n+1)) 2 - 1
                        bottom_size = high - low
                        max_lsize = div bottom_size 2
                        lsize = div (low - 1) 2 + min (n - low) max_lsize
                        rsize = div (low - 1) 2 + max (n - (low + max_lsize)) 0
                        l = completeBinaryTree lsize
                        r = completeBinaryTree rsize

