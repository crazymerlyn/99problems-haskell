import System.Random
import Data.List (nub, (\\), sortBy, elemIndices)

-- Problem 21
-- insertAt
-- Insert an element at a given position in a list
insertAt :: a -> [a] -> Int -> [a]
insertAt _     _  0 = error "Index Out of Bounds"
insertAt e    xs  1 = e:xs
insertAt e (x:xs) n = x:insertAt e xs (n-1)


-- Problem 22
-- range
-- Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range a b = [a..b]


-- Problem 23
-- rndSelect
-- Extract a given number of randomly selected elements from a list
rndSelect :: [a] -> Int -> [a]
rndSelect xs n = take n . map (xs !!) $ randomRs (0, length xs -1) $ mkStdGen 0


-- Problem 24
-- diffSelect
-- Draw n different random numbers from the set 1..m
diffSelect :: Int -> Int -> [Int]
diffSelect n m = take n . nub $ randomRs (1, m) $ mkStdGen 0


-- Problem 25
-- rndPermu
-- Generate a random permutation of the elements of a list
rndPermu :: [a] -> [a]
rndPermu xs = map ((xs !!) . pred) $ diffSelect n n where n = length xs


-- Problem 26
-- combinations
-- Generate the combinations of k distinct objects chosen from the n elements of a list
combinations :: Int -> [a] -> [[a]]
combinations 0  _ = []
combinations 1 xs = [[x] | x <- xs]
combinations n xs = concat [map (xs!!(i-1):) (combinations (n-1) (drop i xs)) | i <- [1..length xs - 1]]


-- Problem 27
-- group
-- Group the elements of a set into disjoint subsets
group :: Eq a => [Int] -> [a] -> [[[a]]]
group [] _ = []
group [x] ys = [[comb] | comb <- (combinations x ys)]
group (x:xs) ys = [(comb:others)| comb <- (combinations x ys), others <- (group xs (ys \\ comb))]


-- Problem 28
-- lsort
-- Sort a list of lists according to length of sublists
lsort :: [[a]] -> [[a]]
lsort = sortBy (\x y -> compare (length x) (length y))

-- lfsort
-- Sort a list of lists according to the frequency of length of sublists
lfsort :: [[a]] -> [[a]]
lfsort xss = sortBy (\xs ys -> compare (freq xs) (freq ys)) xss where
                    lengths = map length xss
                    freq xs = length $ elemIndices (length xs) lengths

