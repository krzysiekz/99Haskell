module MultiwayTrees.Problem71 where

-- (*) Determine the internal path length of a tree.
--
-- We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree.
-- By this definition, tree5 has an internal path length of 9.
--
-- Example in Haskell:
--
-- Tree> ipl tree5
-- 9
-- Tree> ipl tree4
-- 2

import MultiwayTrees.Problem70C

ipl :: Tree a -> Int
ipl = ipl' 0
  where ipl' d (Node _ c) = d + sum (map (ipl' (d + 1)) c)