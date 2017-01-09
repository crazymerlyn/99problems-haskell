import System.Random
import Data.List (nub)

insertAt :: a -> [a] -> Int -> [a]
insertAt _     _  0 = error "Index Out of Bounds"
insertAt e    xs  1 = e:xs
insertAt e (x:xs) n = x:insertAt e xs (n-1)


range :: Int -> Int -> [Int]
range a b = [a..b]


rndSelect :: [a] -> Int -> [a]
rndSelect xs n = take n . map (xs !!) $ randomRs (0, length xs -1) $ mkStdGen 0


diffSelect :: Int -> Int -> [Int]
diffSelect r n = take r . nub $ randomRs (1, n) $ mkStdGen 0

rndPermu :: [a] -> [a]
rndPermu xs = map ((xs !!) . pred) $ diffSelect n n where n = length xs

combinations :: Int -> [a] -> [[a]]
combinations 0  _ = []
combinations 1 xs = [[x] | x <- xs]
combinations n xs = concat [map (xs!!(i-1):) (combinations (n-1) (drop i xs)) | i <- [1..length xs - 1]]


