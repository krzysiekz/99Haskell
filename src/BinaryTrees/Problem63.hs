module BinaryTrees.Problem63 where

-- Construct a complete binary tree
--
-- A complete binary tree with height H is defined as follows:
--
-- The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
-- In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted".
-- This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors
-- (the nil's which are not really nodes!) come last.
-- Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
--
-- We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order,
-- starting at the root with number 1. For every node X with address A the following property holds: The address of X's left
-- and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly construct a complete binary tree structure.
--
-- Write a predicate complete_binary_tree/2.
--
-- Example:
--
-- % complete_binary_tree(N,T) :- T is a complete binary tree with N nodes.
-- Example in Haskell:
--
-- Main> completeBinaryTree 4
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
--
-- Main> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
-- True

import BinaryTrees.Problem55

completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree 1 = Branch 'x' Empty Empty
completeBinaryTree n
    | even n = Branch 'x' (completeBinaryTree (n `div` 2)) (completeBinaryTree ((n - 1) `div` 2))
    | odd n = Branch 'x' (completeBinaryTree ((n - 1) `div` 2)) (completeBinaryTree ((n - 1) `div` 2))

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree tree = isCompleteBinaryTree' (height tree) tree

isCompleteBinaryTree' :: Int -> Tree a -> Bool
isCompleteBinaryTree' 1 _ = True
isCompleteBinaryTree' 2 (Branch _ Empty Empty) = True
isCompleteBinaryTree' 2 (Branch _ left Empty) = True
isCompleteBinaryTree' 2 (Branch _ Empty right) = False
isCompleteBinaryTree' 2 (Branch _ left right) = True
isCompleteBinaryTree' 2 Empty = False
isCompleteBinaryTree' n (Branch _ left right) = isCompleteBinaryTree' (n-1) left && isCompleteBinaryTree' (n-1) right
isCompleteBinaryTree' n _ = False

height :: Tree a -> Int
height Empty = 0
height (Branch _ left right) = if leftHeight > rightHeight then leftHeight else rightHeight
    where leftHeight = 1 + height left
          rightHeight = 1 + height right