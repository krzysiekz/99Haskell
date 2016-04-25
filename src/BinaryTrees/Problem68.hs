module BinaryTrees.Problem68 where

-- Preorder and inorder sequences of binary trees. We consider binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.
--
-- a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in problem P67.
--
-- b) Can you use preorder/2 from problem part a) in the reverse direction; i.e. given a preorder sequence, construct a corresponding tree? If not, make the necessary arrangements.
--
-- c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously. Write a predicate pre_in_tree/3 that does the job.
--
-- Example in Haskell:
--
-- Main> let { Just t = stringToTree "a(b(d,e),c(,f(g,)))" ;
--             po = treeToPreorder t ;
--             io = treeToInorder t } in preInTree po io >>= print
-- Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))


import BinaryTrees.Problem55
import BinaryTrees.Problem67A

treeToPreOrder :: Tree a -> [a]
treeToPreOrder Empty = []
treeToPreOrder (Branch val left right) = val : treeToPreOrder left ++ treeToPreOrder right

treeToInOrder :: Tree a -> [a]
treeToInOrder Empty = []
treeToInOrder (Branch val left right) =  treeToInOrder left ++ [val] ++ treeToInOrder right

preInTree :: (Eq a) => [a] -> [a] -> Tree a
preInTree [] _ = Empty
preInTree _ [] = Empty
preInTree (x:xs) io = Branch x (preInTree poLeft ioLeft) (preInTree poRight ioRight')
    where (ioLeft, ioRight) = span (/=x) io
          ioRight' = filter (/=x) ioRight
          (poLeft, poRight) = span (`elem` ioLeft) xs