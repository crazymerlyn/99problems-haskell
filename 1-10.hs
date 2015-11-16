myLast :: [a] -> a
myLast     [] = error "Not enough elements in list"
myLast    [x] = x
myLast (x:xs) = myLast xs


myButLast :: [a] -> a
myButLast = myLast . init


elementAt :: [a] -> Int -> a
elementAt     [] _ = error "Out of Bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)


myLength :: [a] -> Int
myLength     [] = 0
myLength (x:xs) = 1 + myLength xs


myReverse :: [a] -> [a]
myReverse     [] = []
myReverse (x:xs) = myReverse xs ++ [x]


isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs


data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem  x) = [x]
flatten (List xs) = concat $ map flatten xs


compress :: Eq a => [a] -> [a]
compress           [] = []
compress          [x] = [x]
compress (x:xs@(y:_)) = if x == y then compress xs else x:compress xs


pack :: Eq a => [a] -> [[a]]
pack           [] = []
pack          [x] = [[x]]
pack (x:xs@(y:_)) = case x == y of
                        True -> [x: head rest] ++ tail rest where
                            rest = pack xs
                        False -> [[x]] ++ pack xs


encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(myLength copies, head copies) | copies <- pack xs]

