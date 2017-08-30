import Data.List

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])] deriving (Show, Eq)

data Friendly a = Edge [(a, a)] deriving (Show, Eq)

-- Problem 80
-- Conversions
-- graphToAdj, adjToGraph, graphToFri, friToGraph, adjToFri, friToAdj
graphToAdj :: Eq a => Graph a -> Adjacency a
graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph (x:xs) edges) = Adj ((x, connectedWithX):remainingAdjacencies) where
                                f (a, b) = if a == x then [b] else if b == x then [a] else []
                                connectedWithX = concatMap f edges
                                Adj remainingAdjacencies = graphToAdj (Graph xs edges)

adjToGraph :: Eq a => Adjacency a -> Graph a
adjToGraph (Adj []) = Graph [] []
adjToGraph (Adj ((x, ys):rest)) = Graph (x:xs) (edgesWithX ++ remainingEdges) where
                                        Graph xs remainingEdges = adjToGraph (Adj rest)
                                        f y = if elem (x, y) remainingEdges || elem (y, x) remainingEdges then [] else [(x, y)]
                                        edgesWithX = concatMap f ys


graphToFri :: Eq a => Graph a -> Friendly a
graphToFri (Graph xs edges) = Edge (edges ++ lonelies) where
                                lonelies = [(x, x)| x <- xs, not (x `elem` (map fst edges) || x `elem` (map snd edges))]


friToGraph :: Eq a => Friendly a -> Graph a
friToGraph (Edge []) = Graph [] []
friToGraph (Edge ((x, y):rest)) = Graph nodes edges where
                                    Graph remainingNodes remainingEdges = friToGraph (Edge rest)
                                    nodesWithX = if x `elem` remainingNodes then remainingNodes else (x:remainingNodes)
                                    nodes = if y `elem` nodesWithX then nodesWithX else (y:nodesWithX)
                                    edges = if x == y then remainingEdges else (x,y):remainingEdges


adjToFri :: Eq a => Adjacency a -> Friendly a
adjToFri = graphToFri . adjToGraph

friToAdj :: Eq a => Friendly a -> Adjacency a
friToAdj = graphToAdj . friToGraph


-- Problem 81
-- paths
-- Paths from one node to another one
paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths x y zs = _paths [y] x zs where
                _paths ys@(y:_) x zs = if x == y then [ys] else concatMap f zs where
                                        f (x1, y1) = if y1 == y && not (x1 `elem` ys) then _paths (x1:ys) x zs else []



-- Problem 82
-- cycle
-- Cycle from a given node
cycle :: Eq a => a -> [(a, a)] -> [[a]]
cycle x es = _cycle [x] es where
                _cycle xs es = concatMap f es where
                    last = xs !! (length xs - 1)
                    first = xs !! 0
                    f (x, y) = if x == last && y == first then [xs ++ [y]] else
                               if x == last && not (y `elem` xs) then _cycle (xs ++ [y]) es else []

-- Problem 83
-- spantrees
-- Construct all spanning trees
spantrees :: (Eq a, Ord a) => Graph a -> [Graph a]
spantrees (Graph [] _) = []
spantrees (Graph [x] _) = [Graph [x] []]
spantrees (Graph xs es) = nubBy (\(Graph _ es1) (Graph _ es2) -> sort es1 == sort es2) $ concat [concatMap (f x) (gs x) | x <- xs] where
                                without x (a, b) = a /= x && b /= x
                                gs x = spantrees (Graph (filter (/= x) xs) (filter (without x) es))
                                f v (Graph vs es2) = [Graph (v:vs) (e2:es2) | e2<-es, not (without v e2)]

k4 = Graph ['a', 'b', 'c', 'd']
     [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

