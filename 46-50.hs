import Data.List (intercalate)

-- Problem 46
-- Define predicates and, or, nand, nor, xor, impl, equ, not
not' :: Bool -> Bool
not' True = False
not' False = True


and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool

and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nand' True True = False
nand' _ _ = True

nor' False False = True
nor' _ _ = False

xor' True False = True
xor' False True = False
xor' _ _ = False

impl' True False = False
impl' _ _ = True

equ' a b = not $ xor' a b

-- Write a function table which returns the truth table of a given logical
-- expression in two variables
table :: (Bool -> Bool -> Bool) -> [String]
table f = map showEntry [(a, b, f a b) | a <- [True, False], b <- [True, False]] where
            showEntry (a, b, c) = show a ++ " " ++ show b ++ " " ++ show c



-- Problem 48
-- tablen
-- Generalize the table function to work with any number of logical variables
tablen :: Int -> ([Bool] -> Bool) -> [String]
tablen n f = map showEntry [vs ++ [f vs] | vs <- generateBooleanSampleSpace n] where
                showEntry = intercalate " " . map show
                generateBooleanSampleSpace 1 = [[True], [False]]
                generateBooleanSampleSpace n = map (True:) rest ++ map (False:) rest where
                    rest = generateBooleanSampleSpace (n - 1)


-- Problem 49
-- gray
-- Generate a list of gray codes of given number of bits
gray :: Int -> [String]
gray 0 = []
gray 1 = ["0", "1"]
gray n = map ('0':) rest ++ reverse (map ('1':) rest) where rest = gray (n-1)


-- Problem 50
-- huffman
-- Create huffman codes for given frequency distribution
huffman :: [(Char, Int)] -> [(Char, String)]
huffman = error "TODO: huffman algorithm"
