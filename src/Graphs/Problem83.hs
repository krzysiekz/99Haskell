module Graphs.Problem83 (spanTree) where

-- (**) Construct all spanning trees
--
-- Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph.
-- With this predicate, find out how many spanning trees there are for the graph depicted to the left.
-- The data of this example graph can be found in the file p83.dat. When you have a correct solution for the s_tree/2 predicate,
-- use it to define two other useful predicates: is_tree(Graph) and is_connected(Graph). Both are five-minutes tasks!
--
-- Example in Haskell:
--
-- length $ spantree k4
-- 16

import Data.List

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq, Ord)

k4 = Graph ['a', 'b', 'c', 'd'] [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

spanTree :: (Eq a, Ord a) => Graph a -> [[(a,a)]]
spanTree (Graph v e) = nub $ map sort $ spanTree' [head v] (tail v) e

spanTree' :: (Eq a) => [a] -> [a] -> [(a,a)] -> [[(a,a)]]
spanTree' _ [] _ = [[]]
spanTree' visited unvisited edges = concatMap (\edge@(x,y) -> map (edge:) (spanTree' (newVisited x y visited) (newUnvisited x y unvisited) (newEgdes edge edges))) reachable
    where reachable = filter (\(x,y) -> (x `elem` visited || y `elem` visited) && (x `elem` unvisited || y `elem` unvisited)) edges
          newVisited x y oldVisited = (if x `elem` oldVisited then y else x):oldVisited
          newUnvisited x y = filter (\u -> if x `elem` visited then u /= y else u /= x)
          newEgdes edge = filter (/=edge)
