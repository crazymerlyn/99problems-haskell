import Data.List (intercalate)
import GHC.Exts (sortWith)

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
huffman freqs = let tree = huffmanTree freqs in [(c, getCode tree c)| (c,n)<- freqs]

data HuffmanTree = Leaf Char Int| Head [Char] Int  HuffmanTree HuffmanTree

freqToLeaf :: (Char, Int) -> HuffmanTree
freqToLeaf (c, n) = Leaf c n

getChars :: HuffmanTree -> [Char]
getChars (Leaf c _) = [c]
getChars (Head cs _ _ _) = cs

getFreq :: HuffmanTree -> Int
getFreq (Leaf _ n) = n
getFreq (Head _ n _ _) = n

joinTree :: HuffmanTree -> HuffmanTree -> HuffmanTree
joinTree a b = Head (getChars a ++ getChars b) (getFreq a + getFreq b) a b


huffmanTree :: [(Char, Int)] -> HuffmanTree
huffmanTree freqs = huffmanTree' (sortWith getFreq (map freqToLeaf freqs)) where
                        huffmanTree' [x] = x
                        huffmanTree' (x:y:zs) = huffmanTree'(sortWith getFreq ((joinTree x y):zs))


getCode :: HuffmanTree -> Char -> String
getCode (Leaf c1 _) c2 = case c1 == c2 of
                            True -> ""
                            False -> error "Invalid Tree"
getCode (Head cs _ l r) c = case c `elem` (getChars l) of
                                True -> '0':getCode l c
                                False -> case c `elem` (getChars r) of
                                    True -> '1':getCode r c
                                    False -> error "Character not found"

