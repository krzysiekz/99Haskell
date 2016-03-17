module BinaryTrees.Problem64 where

-- Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for drawing the tree,
-- a layout algorithm is required to determine the position of each node in a rectangular grid.
--
-- In this layout strategy, the position of a node v is obtained by the following two rules:
--
-- x(v) is equal to the position of the node v in the inorder sequence
-- y(v) is equal to the depth of the node v in the tree
-- Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.
--
-- Here is the example tree from the above illustration:
--
import Data.List
import BinaryTrees.Problem55

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )
-- Example in Haskell:
--
-- > layout tree64
-- Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...

layout :: Tree a -> [(a,(Int, Int))]
layout = layout' 0 0

layout' :: Int -> Int -> Tree a -> [(a,(Int, Int))]
layout' _ _ Empty = []
layout' n h (Branch val Empty Empty) = [(val, (1+n, 1+h))]
layout' n h (Branch val left right) = leftResult ++ [(val, (x + 1, h+1))] ++ rightResult
    where leftResult = layout' n (h+1) left
          x = if not (null leftResult) then fst $ snd $ maxX leftResult else n
          rightResult = layout' (1 + x) (h + 1) right

maxX :: [(a, (Int, Int))] -> (a, (Int, Int))
maxX = maximumBy (\ (_, (x,_)) (_, (x2,_)) -> compare x x2)