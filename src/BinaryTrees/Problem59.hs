module BinaryTrees.Problem59 where

-- (**) Construct height-balanced binary trees
--
-- In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the
-- height of its right subtree are almost equal, which means their difference is not greater than one.
--
-- Construct a list of all height-balanced binary trees with the given element and the given maximum height.
--
-- Example:
--
-- ?- hbal_tree(3,T).
-- T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
-- T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
-- etc......No
-- Example in Haskell:
--
-- *Main> take 4 $ hbalTree 'x' 3
-- [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]

import BinaryTrees.Problem55

hbalTree :: (Eq a, Ord a) => a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree v 1 = [Branch v Empty Empty]
hbalTree v n = [Branch v left right | (l, r) <- [(n-2, n-1), (n-1, n-1), (n-1, n-2)], left <- hbalTree v l, right <- hbalTree v r]