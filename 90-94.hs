-- Problem 90
-- queens
-- Solve the eight queens problem
queens :: Int -> [[Int]]
queens n = recurse [] where
            recurse sofar = if length sofar == n then
                [reverse sofar] else
                concatMap recurse possibs where
                    allpossibs = [(x:sofar)| x <- [1..n]]
                    possibs = filter isvalid allpossibs
                    isvalid (x:xs) = notElem x xs && all id [abs (x - y) /= i| (y, i) <- zip xs [1..]]
