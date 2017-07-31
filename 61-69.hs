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


-- Problem 64
-- layout
-- Write a function to annonate each node of the tree with a position,
-- where (1, 1) is top left corner
layout :: Tree a -> Tree (a, (Int, Int))
layout t = _layout t 1 1 where
            _layout Empty _ _ = Empty
            _layout (Branch x l r) order depth = Branch (x, (order + countNodes l, depth)) _l _r where
                                                    countNodes Empty = 0
                                                    countNodes (Branch _ l r) = 1 + countNodes l + countNodes r
                                                    _l = _layout l order (depth + 1)
                                                    _r = _layout r (order + countNodes l + 1) (depth + 1)



-- Problem 65
-- layout2
-- Write a function to annonate each node of the tree with a position,
-- where (1, 1) is top left corner
layout2 :: Tree a -> Tree (a, (Int, Int))
layout2 t = _layout2 t (2 ^ h - 1) 1 where
                height Empty = 0
                height (Branch _ l r) = 1 + max (height l) (height r)
                h = height t - 1
                _layout2 Empty _ _ = Empty
                _layout2 (Branch elem l r ) x y = Branch (elem, (x, y)) _l _r where
                    d = div (x + 1) 2
                    _l = _layout2 l (x - d) (y+1)
                    _r = _layout2 r (x + d) (y+1)


-- Problem 67A
-- stringToTree, treeToString
-- A string representation of binary trees
stringToTree :: [Char] -> Tree Char
stringToTree s = let (x, y) = _help s in x where
                _help "" = (Empty, "")
                _help [a] = (Branch a Empty Empty, "")
                _help (',':rs) = (Empty, rs)
                _help (')':rs) = (Empty, rs)
                _help (a:b:rs) = case b of
                    '(' -> (Branch a l r, remaining) where
                        (r, remaining) = _help after_left
                        (l, after_left) = _help rs
                    ')' -> case rs of 
                        ',':rss   -> (Branch a Empty Empty, rss)
                        otherwise -> (Branch a Empty Empty, rs)
                    ',' -> (Branch a Empty Empty, rs)

treeToString :: Tree Char -> [Char]
treeToString Empty = ""
treeToString (Branch c Empty Empty) = [c]
treeToString (Branch c l r) = [c] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"


-- Problem 68
-- preorder, inorder, preInTree
-- Write functions to get the preorder and inorder traversal of a tree
-- Write a function to get the original tree from preorder and inorder traversals
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Branch x l r) = [x] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Branch x l r) = inorder l ++ [x] ++ inorder r

preInTree :: Eq a => [a] -> [a] -> Tree a
preInTree [] [] = Empty
preInTree po@(x:xs) io = Branch x l r where
                            (lio, _:rio) = break (==x) io
                            (lpo, rpo) = splitAt (length lio) xs
                            l = preInTree lpo lio
                            r = preInTree rpo rio

