module BinaryTrees.Problem61A where

-- Collect the leaves of a binary tree in a list
--
-- A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.
--
-- Example:
--
-- % leaves(T,S) :- S is the list of all leaves of the binary tree T
-- Example in Haskell:
--
-- > leaves tree4
-- [4,2]

import BinaryTrees.Problem55
import BinaryTrees.Problem61

leaves :: Tree a -> [a]
leaves Empty = []
leaves l@(Branch v Empty Empty) = [v]
leaves (Branch _ left right) = leaves left ++ leaves right
