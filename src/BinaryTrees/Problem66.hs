module BinaryTrees.Problem66 where

-- Yet another layout strategy is shown in the illustration below:
--
-- https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p66.gif
--
-- The method yields a very compact layout while maintaining a certain symmetry in every node.
-- Find out the rules and write the corresponding Prolog predicate. Hint:
-- Consider the horizontal distance between a node and its successor nodes.
-- How tight can you pack together two subtrees to construct the combined binary tree?
--
-- Use the same conventions as in problem P64 and P65 and test your
-- predicate in an appropriate way. Note: This is a difficult problem.
-- Don't give up too early!
--
-- Which layout do you like most?
--
-- Example in Haskell:
--
-- > layout tree65
-- Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...

import BinaryTrees.Problem55
import qualified  BinaryTrees.Problem65 as S
import qualified Data.List as List

layout :: Tree a -> [(a,(Int, Int))]
layout = layout' 0 0

layout' :: Int -> Int -> Tree a -> [(a,(Int, Int))]
layout' _ _ Empty = []
layout' n h (Branch val Empty Empty) = [(val, (1+n, 1+h))]
layout' n h (Branch val left right) =[(val, (diffMiddle , h+1))] ++ leftResult ++ rightResult
    where leftResult = layout' n (h+1) left
          x = if not (null leftResult) then fst $ snd $ head leftResult else n
          rightResult = layout' diffRight (h + 1) right
          maxR = maxRight left
          minL = minLeft right
          diffMiddle = x + if maxR == 0 || minL == 0 then 1 else maxR + minL
          diffRight = if minL == 0 then diffMiddle else x + (maxR + minL)*2 - minL -1

maxX :: [(a, (Int, Int))] -> (a, (Int, Int))
maxX = List.maximumBy (\ (_, (x,_)) (_, (x2,_)) -> compare x x2)

minLeft :: Tree a -> Int
minLeft Empty = 0
minLeft (Branch _ Empty _) = 0
minLeft (Branch _ left _) = 1 + minLeft left

maxRight :: Tree a -> Int
maxRight Empty = 0
maxRight (Branch _ _ Empty) = 0
maxRight (Branch _ _ right) = 1 + maxRight right