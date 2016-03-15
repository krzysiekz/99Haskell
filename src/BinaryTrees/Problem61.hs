module BinaryTrees.Problem61 where

-- Count the leaves of a binary tree
--
-- A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.
--
-- Example:
--
-- % count_leaves(T,N) :- the binary tree T has N leaves
-- Example in Haskell:
--
-- > countLeaves tree4
-- 2

import BinaryTrees.Problem55

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left right) = countLeaves left + countLeaves right