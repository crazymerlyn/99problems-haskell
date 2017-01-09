import Data.List

data EncodedElement a = Multiple Int a| Single a deriving Show

-- Problem 11
-- encodeModified
-- Modified run-length encoding
-- Modify the result of problem 10 in such a way that if an element has
-- no duplicates, it is simply copied into the result list.
-- Only elements with duplicates are transfered as (N E) lists.
encodeModified :: Eq a => [a] -> [EncodedElement a]
encodeModified xs = map f' $ group xs where
                        f' [x] = Single x
                        f'  xs = Multiple (length xs) (head xs)


-- Problem 12
-- decodeModified
-- Decode a run-length encoded list
decodeModified :: Eq a => [EncodedElement a] -> [a]
decodeModified xs = concat $ map decodeElem xs where
                        decodeElem (Single     x) = [x]
                        decodeElem (Multiple n x) = replicate n x


-- Problem 13
-- encodeDirect
-- Run-length encoding of a list (direct solution)
-- Implement the run-length encoding data compression method directly
-- i.e. don't explicitly create the sublists containing the duplicates,
-- but only count them
encodeDirect :: Eq a => [a] -> [EncodedElement a]
encodeDirect           [] = []
encodeDirect          [x] = [Single x]
encodeDirect (x:xs@(y:_)) = case x == y of
                                True -> case encodeDirect xs of
                                    (Multiple n _):rest -> (Multiple (n+1) x): rest
                                    (Single _):rest -> (Multiple 2 x) : rest
                                False -> (Single x) : encodeDirect xs


-- Problem 14
-- Duplicate the elements of a list
dupli :: [a] -> [a]
dupli     [] = []
dupli (x:xs) = x:x:dupli xs


-- Problem 15
-- repli
-- Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli     [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n


-- Problem 16
-- dropEvery
-- Drop every nth element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n 1 where
                    dropEvery'     [] _ _ = []
                    dropEvery' (x:xs) n r = case r == n of
                                                True  -> dropEvery' xs n 1
                                                False -> x:dropEvery' xs n (r+1)


-- Problem 17
-- split
-- Split a list into two parts, the length of the first part is given
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)


-- Problem 18
-- slice
-- Extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice xs low high = take (high - low + 1) . drop (low - 1) $ xs


-- Problem 19
-- rotate
-- Rotate a list n places to the left
rotate :: [a] -> Int -> [a]
rotate xs n
    |    n >= 0 = drop n xs ++ take n xs
    | otherwise = rotate xs (length xs + n)


-- Problem 20
-- removeAt
-- Remove the nth element from a list
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)

