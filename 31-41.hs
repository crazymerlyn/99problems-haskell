-- Problem 31
-- isPrime
-- Determine whether a given integer number is prime
isPrime :: Int -> Bool
isPrime n
    | n < 0     = isPrime (-n)
    | n < 2     = False
    | otherwise = and [n `mod` i /= 0 | i <- [2..limit]] where limit = truncate $ sqrt (fromIntegral n)


-- Problem 32
-- mygcd
-- Determine the greatest common divisor of two positive integer numbers.
-- Use euclid's algorithm
mygcd :: Int -> Int -> Int
mygcd a 0 = abs a
mygcd a b = mygcd b (a `mod` b)


-- Problem 33
-- coprime
-- Determine if two integers are coprime
-- Two numbers are coprime if the greatest common divisor is 1
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1


-- Problem 34
-- totientPhi
-- Calculate Euler's totient phi(m)
totientPhi :: Int -> Int
totientPhi m = length . filter (coprime m) $ [1..m]


-- Problem 35
-- primeFactors
-- Determine the prime factors of a given positive integer
-- Construct a flat list containing the prime factors in ascending order
primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2 where
                    primeFactors' n m = case n < m of
                        True -> case n == 1 of
                            True -> []
                            False -> [n]
                        False -> case n `rem` m of
                            0 -> (m:primeFactors' (n `div` m) m)
                            _ -> primeFactors' n (m+1)


-- Problem 36
-- primeFactorsMul
-- Determine the prime fators of a given positive integer
-- Contruct a list containing the prime factors and their multiplicity
primeFactorsMul :: Int -> [(Int, Int)]
primeFactorsMul n = packPrimes (primeFactors n) where
                        packPrimes [] = []
                        packPrimes xs@(x:_) = ((x, l):packPrimes rest) where
                            l = length $ takeWhile (x==) xs
                            rest = dropWhile (x==) xs


-- Problem 37
-- totientPhi'
-- Calculate Euler's totient phi function using prime factorization
totientPhi' :: Int -> Int
totientPhi' n = product $ [(p - 1) * (p ^ (m - 1)) | (p,m) <- primeFactorsMul n]


-- Problem 38
-- Compare the two methods of calculating Euler's totient function
-- Solution: The second method is much faster for larger numbers


-- Problem 39
-- primesR
-- Construct a list of prime numbers with given lower and upper limit
primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]


-- Problem 40
-- goldbach
-- Goldbach's Conjecture
-- Find two prime integers whose sum is the given integer
goldbach :: Int -> (Int, Int)
goldbach n = goldbach' n (primesR 1 n) where
                goldbach' n [] = (-1,-1)
                goldbach' n t@(x:xs) = case (n - x) `elem` t of
                    True -> (x, n-x)
                    False -> goldbach' n xs


-- Problem 41
-- goldbachList
-- Find the goldbach composition of all even integers in a given range
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach $ dropWhile (<4) $ filter even [a..b]

-- goldbahcList'
-- Find the number of integers whose goldbach decomposition has numbers greate
-- than the specified limit
goldbachList' :: Int -> Int -> Int -> Int
goldbachList' a b limit = length $ filter (\(x,y) -> x > limit) (goldbachList a b)

