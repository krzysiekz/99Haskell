module BinaryTrees.Problem58 where

-- (**) Generate-and-test paradigm
--
-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
--
-- Example:
--
-- * sym-cbal-trees(5,Ts).
-- Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]
-- Example in Haskell:
--
-- *Main> symCbalTrees 5
-- [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]

import BinaryTrees.Problem55
import BinaryTrees.Problem56

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree