module BinaryTrees.Problem56 where

-- (**) Symmetric binary trees
--
-- Let us call a binary tree symmetric if you can draw a vertical line through the root node and then
-- the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check
-- whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether
-- one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
--
-- Example in Haskell:
--
-- *Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
-- False
-- *Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
-- True

import BinaryTrees.Problem55

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = symmetric' left right

symmetric' :: (Eq a) => Tree a -> Tree a -> Bool
symmetric' Empty Empty = True
symmetric' (Branch _ l1 r1) (Branch _ l2 r2) = symmetric' l1 r2 && symmetric' r1 l2
symmetric' _ _ = False
