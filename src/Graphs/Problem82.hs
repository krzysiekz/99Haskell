module Graphs.Problem82 where

-- (*) Cycle from a given node
--
-- Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G.
-- The predicate should return all cycles via backtracking.
--
-- Example in Haskell:
--
-- graph> cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- [[2,3,4,2]]
-- graph> cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- []

cycle' :: (Eq a) => a -> [(a, a)] -> [[(a, a)]]
cycle' node allPaths = filter ((==node) . snd . last) $ cycle'' node node allPaths
    where cycle'' :: (Eq a) => a -> a -> [(a, a)] -> [[(a, a)]]
          cycle'' s d p = map (\path@(x,y) -> path : concat ( cycle'' y d (filter (/=path) p)) ) findPath
                where findPath = filter ( (==s) . fst) p
