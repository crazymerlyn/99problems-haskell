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


