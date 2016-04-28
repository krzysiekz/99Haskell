module MultiwayTrees.Problem70 (treeToString, stringToTree) where

-- (**) Tree construction from a node string.
--
-- We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes,
-- a special character ^ has been inserted whenever,
-- during the tree traversal, the move is a backtrack to the previous level.
--
-- By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^
--
-- https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p70.gif
--
-- Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given. Make your predicate work in both directions.
--
-- Example in Haskell:
--
-- Tree> stringToTree "afg^^c^bd^e^^^"
-- Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
--
-- Tree> treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])
-- "afg^^c^bd^e^^^"

import MultiwayTrees.Problem70C
import Data.Char

treeToString :: Tree Char -> String
treeToString node = treeToString' node ++ "^"
    where treeToString' (Node val c) = val : foldl (\acc e -> acc ++ treeToString' e ++ ['^']) [] c

stringToTree :: String -> Tree Char
stringToTree [] = error "Cannot build tree from empty string"
stringToTree (x:xs) = Node x $ map stringToTree $ partitionNodes $ init xs

partitionNodes :: String -> [String]
partitionNodes [] = []
partitionNodes s = node : partitionNodes rest
    where (node, rest) = takeNode s 0

takeNode :: String -> Int -> (String, String)
takeNode [] _ = ([],[])
takeNode (x:xs) n
    | isAlphaNum x = let (node, rest) = takeNode xs (n+1) in (x:node, rest)
    | x == '^' && n > 1 = let (node, rest) = takeNode xs (n-1) in (x:node, rest)
    | otherwise = ([x], xs)