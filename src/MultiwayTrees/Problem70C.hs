module MultiwayTrees.Problem70C where

data Tree a = Node a [Tree a]
        deriving (Eq, Show)

-- Some example trees:

tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

-- (*) Count the nodes of a multiway tree.
--
-- Example in Haskell:
--
-- Tree> nnodes tree2
-- 2

nNodes :: Tree a -> Int
nNodes (Node v c) = 1 + foldl (\acc e -> acc + nNodes e) 0 c