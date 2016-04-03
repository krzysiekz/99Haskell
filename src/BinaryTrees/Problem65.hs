module BinaryTrees.Problem65 where

-- An alternative layout method is depicted in the illustration below:

-- https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p65.gif
--
-- Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance between neighboring nodes is constant.
--
-- Use the same conventions as in problem P64 and test your function in an appropriate way.
--
-- Here is the example tree from the above illustration:

import BinaryTrees.Problem55
import Data.List

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

-- Example in Haskell:
--
-- > layout tree65
-- Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...

layout :: Tree a -> [(a,(Int, Int))]
layout tree = layout' startingX 0 (treeHeight tree) tree
    where startingX = 2^leftHeight tree -1

layout' :: Int -> Int -> Int -> Tree a -> [(a,(Int, Int))]
layout' _ _ _ Empty = []
layout' n lev h (Branch val Empty Empty) = [(val, (n, 1+lev))]
layout' n lev h (Branch val left right) = [(val, (n, lev+1))] ++ leftResult ++ rightResult
    where diff = 2^(h-lev-1) `div` 2
          leftResult = layout' (n-diff) (lev+1) h left
          rightResult = layout' (n+diff) (lev + 1) h right

maxX :: [(a, (Int, Int))] -> (a, (Int, Int))
maxX = maximumBy (\ (_, (x,_)) (_, (x2,_)) -> compare x x2)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Branch _ left right) = 1 + max (treeHeight left) (treeHeight right)

leftHeight :: Tree a -> Int
leftHeight Empty = 0
leftHeight (Branch _ left _) = 1 + leftHeight left
