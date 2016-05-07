module MultiwayTrees.Problem73
(lisp, lispToTree)
where

-- (**) Lisp-like tree representation.
--
-- There is a particular notation for multiway trees in Lisp. Lisp is a prominent functional programming language,
-- which is used primarily for artificial intelligence problems. As such it is one of the main competitors of Prolog.
-- In Lisp almost everything is a list, just as in Prolog everything is a term.
--
-- The following pictures show how multiway tree structures are represented in Lisp.
--
-- p73.png
--
-- Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list,
-- followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')',
-- which we shall collectively call "tokens". We can represent this sequence of tokens as a Prolog list;
-- e.g. the lispy expression (a (b c)) could be represented as the Prolog list ['(', a, '(', b, c, ')', ')'].
-- Write a predicate tree_ltl(T,LTL) which constructs the "lispy token list" LTL if the tree is given as term T in the usual Prolog notation.
--
-- (The Prolog example given is incorrect.)
--
-- Example in Haskell:
--
-- Tree> display lisp tree1
-- "a"
-- Tree> display lisp tree2
-- "(a b)"
-- Tree> display lisp tree3
-- "(a (b c))"
-- Tree> display lisp tree4
-- "(b d e)"
-- Tree> display lisp tree5
-- "(a (f g) c (b d e))"
-- As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that the inverse conversion is also possible.

import MultiwayTrees.Problem70C
import Data.List
import Data.Char

lisp :: Tree Char -> String
lisp (Node val []) = [val]
lisp (Node val c) = "(" ++ [val] ++ concatMap lisp c ++ ")"

lispToTree :: String -> Tree Char
lispToTree s = lispToTree' $ head $ partitionNodes s

lispToTree' :: String -> Tree Char
lispToTree' [] = error "Cannot build tree from empty string"
lispToTree' [x] = Node x []
lispToTree' (x:xs) = Node x $ map lispToTree' $ partitionNodes xs

partitionNodes :: String -> [String]
partitionNodes [] = []
partitionNodes s = node : partitionNodes rest
    where (node, rest) = takeNode s 0

takeNode :: String -> Int -> (String, String)
takeNode [] _ = ([],[])
takeNode (x:xs) n
    | isAlphaNum x && n > 0 = let (node, rest) = takeNode xs n in (x:node, rest)
    | x == '(' && n == 0 = let (node, rest) = takeNode xs (n+1) in (node, rest)
    | x == '(' && n > 0 = let (node, rest) = takeNode xs (n+1) in (x:node, rest)
    | x == ')' && n > 1 = let (node, rest) = takeNode xs (n-1) in (x:node, rest)
    | x == ')' && n == 1 =  ([], xs)
    | otherwise = ([x], xs)


