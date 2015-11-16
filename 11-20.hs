import Data.List

data EncodedElement a = Multiple Int a| Single a deriving Show

encodeModified :: Eq a => [a] -> [EncodedElement a]
encodeModified xs = map f' $ group xs where
                        f' [x] = Single x
                        f'  xs = Multiple (length xs) (head xs)


decodeModified :: Eq a => [EncodedElement a] -> [a]
decodeModified xs = concat $ map decodeElem xs where
                        decodeElem (Single     x) = [x]
                        decodeElem (Multiple n x) = replicate n x


encodeDirect :: Eq a => [a] -> [EncodedElement a]
encodeDirect           [] = []
encodeDirect          [x] = [Single x]
encodeDirect (x:xs@(y:_)) = case x == y of
                                True -> case encodeDirect xs of
                                    (Multiple n _):rest -> (Multiple (n+1) x): rest
                                    (Single _):rest -> (Multiple 2 x) : rest
                                False -> (Single x) : encodeDirect xs

dupli :: [a] -> [a]
dupli     [] = []
dupli (x:xs) = x:x:dupli xs


repli :: [a] -> Int -> [a]
repli     [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n


dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n 1 where
                    dropEvery'     [] _ _ = []
                    dropEvery' (x:xs) n r = case r == n of
                                                True  -> dropEvery' xs n 1
                                                False -> x:dropEvery' xs n (r+1)


split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)


slice :: [a] -> Int -> Int -> [a]
slice xs low high = take (high - low + 1) . drop (low - 1) $ xs


rotate :: [a] -> Int -> [a]
rotate xs n
    |    n >= 0 = drop n xs ++ take n xs
    | otherwise = rotate xs (length xs + n)


removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)

