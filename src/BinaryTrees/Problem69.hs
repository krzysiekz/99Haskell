module BinaryTrees.Problem69 where

-- Dotstring representation of binary trees.
--
-- We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.
-- Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal.
-- For example, the tree shown in problem P67 is represented as 'abd..e..c.fg...'.
-- First, try to establish a syntax (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 which does the conversion in both directions. Use difference lists.
--
-- Example in Haskell:
--
-- > fst (ds2tree example)
-- Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
--
-- > tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty))
-- "xy..z0..."


import BinaryTrees.Problem55
import Data.Char as Char

tree2ds :: Tree Char -> String
tree2ds Empty = ['.']
tree2ds (Branch val left right) = val : tree2ds left ++ tree2ds right

ds2tree :: String -> Tree Char
ds2tree ['.'] = Empty
ds2tree (x:xs)
    | isAlphaNum x = Branch x (ds2tree left) (ds2tree right)
    | otherwise = Empty
        where (left, right) = splitNodes xs 0 False

splitNodes :: String -> Int -> Bool -> (String, String)
splitNodes [] _ _ = ([],[])
splitNodes (x:xs) n isChild
    | isAlphaNum x = let (left, right) = splitNodes xs (if isChild then n+1 else n+2) True in (x:left,right)
    | x == '.' && n > 1 = let (left, right) = splitNodes xs (n-1) True in (x:left,right)
    | otherwise = ([x], xs)

