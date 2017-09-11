import Data.List
import Data.Ord

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


-- Problem 84
-- prim
-- Construct the minimum spanning tree
prim :: (Eq a, Ord b) => [a] -> [(a, a, b)] -> [(a, a, b)]
prim [] _ = []
prim xs@(x:_) es = prim' xs es [x] where
                    contains x (a, b, _) = x == a || x == b
                    prim' xs es seen = if length seen == length xs then [] else (nextEdge: prim' xs es (seen ++ [next])) where
                      trd (_,_,w) = w
                      xor a b = (a || b) && (not (a && b))
                      f (a, b, _) = (a `elem` seen) `xor` (b `elem` seen)
                      possibs = filter f es
                      nextEdge = minimumBy (comparing trd) possibs
                      g (a, b, _) = if a `elem` seen then b else a
                      next = g nextEdge


-- Problem 85
-- iso
-- Graph isomorphism
iso :: (Eq a, Ord a, Eq b, Ord b) => Graph a -> Graph b -> Bool
iso g1 g2 = any (\f -> graphEqual (f g1) g2) (makeMappings g1 g2) where
                matching [] x = error "No matching element found"
                matching ((a,b):ys) x = if a == x then b else matching ys x
                g mapping (Graph xs es) = 
                            Graph (map (matching mapping) xs) [(matching mapping a, matching mapping b) | (a,b)<-es]
                makeMappings (Graph xs1 _) (Graph xs2 _) = if length xs1 /= length xs2 then [] else
                                                            [g (zip perm xs2) | perm <- permutations xs1]
                graphEqual (Graph xs1 es1) (Graph xs2 es2) = sort xs1 == sort xs2 && sort es1 == sort es2


-- Problem 86
-- degree, sortedDegrees, kcolor
-- Write a function that determines the degree of a given node.
-- Write a function that generates a list of all nodes of a graph
-- sorted according to decreasing degree.
-- Use Welch-Powell's algorithm to paint the nodes of a graph in such
-- a way that adjacent nodes have different colors.
degree :: Eq a => Graph a -> a -> Int
degree (Graph _ es) x = length $ filter (\(a, b) -> x == a || x == b) es

sortedDegrees :: Eq a => Graph a -> [a]
sortedDegrees g@(Graph xs es) = reverse $ sortBy (comparing (degree g)) xs

kcolor :: (Eq a, Ord a) => Graph a -> [(a, Int)]
kcolor g = kcolor' adj 1 [] where
            adj = sortByDegrees g
            sortByDegrees g = sortBy (comparing (length . snd)) adj' where
                Adj adj' = graphToAdj g
            kcolor' [] _ acc = acc
            kcolor' xs n acc = kcolor' newxs (n+1) acc' where
                newxs = [x | x <- xs, notElem (fst x, n) acc']
                acc' = color xs n acc
                color [] _ acc = acc
                color ((v,e):xs) n acc = if any (\x -> (x, n) `elem` acc) e
                                            then color xs n acc
                                            else color xs n ((v, n):acc)

-- Problem 87
-- depthfirst
-- Depth first order graph traversal (alternative solution)
depthfirst :: Eq a => Graph a -> a -> [a]
depthfirst g x = reverse $ search g x [] where
                    search (Graph xs _) _ seen | length seen == length xs = seen
                    search (Graph xs es) x seen = foldl' combine (x:seen) next where
                        combine sofar node = if notElem node sofar then search g node sofar else sofar
                        next = [y | y <- xs, y `notElem` seen && ((x, y) `elem` es || (y, x) `elem` es)]


-- Problem 88
-- connectedComponents
-- Write a function that splits a graph into its connected components
connectedComponents :: Eq a => Graph a -> [[a]]
connectedComponents (Graph [] _) = []
connectedComponents g@(Graph xs@(x:_) es) = connectedToX : rest where
                                            connectedToX = depthfirst g x
                                            rest = connectedComponents (Graph (xs \\ connectedToX) es)



-- Problem 89
-- bipartite
-- Write a function that finds out if the given graph is bipartite
bipartite :: (Eq a, Ord a) => Graph a -> Bool
bipartite g = length (nub (map snd (kcolor g))) == 2

