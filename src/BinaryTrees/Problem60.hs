module BinaryTrees.Problem60 where

-- (**) Construct height-balanced binary trees with a given number of nodes
--
-- Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?
--
-- Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult.
-- Try to find a recursive statement and turn it into a function minNodes that returns the minimum number
-- of nodes in a height-balanced binary tree of height H. On the other hand, we might ask: what is the maximum
-- height H a height-balanced binary tree with N nodes can have? Write a function maxHeight that computes this.
-- Now, we can attack the main problem: construct all the height-balanced binary trees with a given number
-- of nodes. Find out how many height-balanced trees exist for N = 15.
--
-- Example in Prolog:
--
-- ?- count_hbal_trees(15,C).
-- C = 1553
-- Example in Haskell:
--
-- *Main> length $ hbalTreeNodes 'x' 15
-- 1553
-- *Main> map (hbalTreeNodes 'x') [0..3]
-- [[Empty],
--  [Branch 'x' Empty Empty],
--  [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
--  [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]

import BinaryTrees.Problem55
import BinaryTrees.Problem59

hbalTreeNodes :: (Eq a, Ord a) => a -> Int -> [Tree a]
hbalTreeNodes val n = filter ((== n) . countNodes) $ concatMap (hbalTree val) [min..max]
    where min = ceiling $ logBase 2 $ fromIntegral (n+1)
          max = length (takeWhile (<=n) minNodesSeq) -1

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch v l r) = 1 + countNodes l + countNodes r

--minimal number od nodes needed to get height + 1 in binary tree is
--represented as sequence similar to fibonacci but +1 is added in each step
minNodesSeq = 0:1:zipWith ((+).(1+)) minNodesSeq (tail minNodesSeq)