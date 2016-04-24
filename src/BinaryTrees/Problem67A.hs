module BinaryTrees.Problem67A where

-- A string representation of binary trees
--
-- Somebody represents binary trees as strings of the following type:
--
-- a(b(d,e),c(,f(g,)))
-- a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.
--
-- Example in Prolog
--
-- ?- tree_to_string(t(x,t(y,nil,nil),t(a,nil,t(b,nil,nil))),S).
-- S = 'x(y,a(,b))'
-- ?- string_to_tree('x(y,a(,b))',T).
-- T = t(x, t(y, nil, nil), t(a, nil, t(b, nil, nil)))
-- Example in Haskell:
--
-- Main> stringToTree "x(y,a(,b))" >>= print
-- Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
-- Main> let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)
-- True

import BinaryTrees.Problem55

stringToTree :: String -> Tree Char
stringToTree [] = Empty
stringToTree [x] = Branch x Empty Empty
stringToTree (x:y:xs)
    | y == '(' = Branch x (stringToTree left) (stringToTree right)
    where inner = takeInner xs 0
          (left, right) = spanNodes inner 0

takeInner :: String -> Int -> String
takeInner [] _ = []
takeInner (x:xs) c
    | x == '(' = x : takeInner xs (c+1)
    | x == ')' = if c == 0 then [] else x : takeInner xs (c-1)
    | otherwise = x : takeInner xs c

spanNodes :: String -> Int -> (String, String)
spanNodes [] _ = ([], [])
spanNodes (x:xs) c
    | x == '(' = let (l, r) = spanNodes xs (c+1)  in (x:l, r)
    | x == ')' = let (l, r) = spanNodes xs (c-1)  in (x:l, r)
    | x == ',' && c == 0 = ([], xs)
    | otherwise = let (l, r) = spanNodes xs c  in (x:l, r)


treeToString :: Tree Char -> String
treeToString Empty = []
treeToString (Branch val Empty Empty) = [val]
treeToString (Branch val l r) = val : "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"
