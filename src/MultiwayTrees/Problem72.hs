module MultiwayTrees.Problem72 where

-- (*) Construct the bottom-up order sequence of the tree nodes.
--
-- Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the multiway tree Tree.
--
-- Example in Haskell:
--
-- Tree> bottom_up tree5
-- "gfcdeba"

import MultiwayTrees.Problem70C

bottomUp :: Tree a -> [a]
bottomUp (Node val []) = [val]
bottomUp (Node val c) = concatMap bottomUp c ++ [val]