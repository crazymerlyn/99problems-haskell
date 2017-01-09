-- Problem 1
-- myLast
-- Find the last element of a list
myLast :: [a] -> a
myLast     [] = error "Not enough elements in list"
myLast    [x] = x
myLast (x:xs) = myLast xs


-- Problem 2
-- myButLast
-- Find the last but one element of a list
myButLast :: [a] -> a
myButLast = myLast . init


-- Problem 3
-- elementAt
-- Find the nth element of a list
elementAt :: [a] -> Int -> a
elementAt     [] _ = error "Out of Bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)


-- Problem 4
-- myLength
-- Find the number of elements of a list
myLength :: [a] -> Int
myLength     [] = 0
myLength (x:xs) = 1 + myLength xs


-- Problem 5
-- myReverse
-- Reverse a list
myReverse :: [a] -> [a]
myReverse     [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- Problem 6
-- isPalindrome
-- Find out whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs


-- Problem 7
-- flatten
-- Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem  x) = [x]
flatten (List xs) = concat $ map flatten xs


-- Problem 8
-- compress
-- Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress           [] = []
compress          [x] = [x]
compress (x:xs@(y:_)) = if x == y then compress xs else x:compress xs


-- Problem 9
-- pack
-- Pack consecutive duplicate elements of a list into sublists
pack :: Eq a => [a] -> [[a]]
pack           [] = []
pack          [x] = [[x]]
pack (x:xs@(y:_)) = case x == y of
                        True -> [x: head rest] ++ tail rest where
                            rest = pack xs
                        False -> [[x]] ++ pack xs


-- Problem 10
-- encode
-- Encode a list using run-length encoding
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(myLength copies, head copies) | copies <- pack xs]

