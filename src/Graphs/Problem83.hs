module Graphs.Problem83 where

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

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)
