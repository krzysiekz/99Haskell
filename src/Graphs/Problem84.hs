module Graphs.Problem84 where

-- (**) Construct the minimal spanning tree
--
-- Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal spanning tree of a given labelled graph.
-- Hint: Use the algorithm of Prim. A small modification of the solution of P83 does the trick.
-- The data of the example graph to the right can be found in the file p84.dat.
--
-- Example in Haskell:
--
-- prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
-- [(1,2,12),(1,3,34),(2,4,55),(2,5,32)]

import Data.List

prim :: (Eq a) => [a] -> [(a,a,Int)] -> [(a,a,Int)]
prim v = prim' [head v] (tail v)

prim' :: (Eq a) => [a] -> [a] -> [(a,a,Int)] -> [(a,a,Int)]
prim' _ [] _ = []
prim' visited unvisited edges = closest : prim' (newVisited closest visited) (newUnvisited closest unvisited) (newEdges closest edges)
    where reachable = filter (\(x,y,_) -> (x `elem` visited || y `elem` visited) && (x `elem` unvisited || y `elem` unvisited)) edges
          closest = minimumBy (\ (_,_,x) (_,_,y) -> x `compare` y) reachable
          newVisited (x,y,_) oldVisited = (if x `elem` oldVisited then y else x):oldVisited
          newUnvisited (x,y,_) = filter (\u -> if x `elem` visited then u /= y else u /= x)
          newEdges edge = filter (/=edge)
