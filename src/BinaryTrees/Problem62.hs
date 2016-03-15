module BinaryTrees.Problem62 where

-- Collect the internal nodes of a binary tree in a list
--
-- An internal node of a binary tree has either one or two non-empty successors.
-- Write a predicate internals/2 to collect them in a list.
--
-- Example:
--
-- % internals(T,S) :- S is the list of internal nodes of the binary tree T.
-- Example in Haskell:
--
-- Prelude>internals tree4
-- Prelude>[1,2]

import BinaryTrees.Problem55
import BinaryTrees.Problem61

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch v left right) = [v] ++ internals left ++ internals right