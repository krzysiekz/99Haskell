module Lists.Problem10 where

import Lists.Problem9

-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data
-- compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
--
-- Example:
--
-- * (encode '(a a a a b c c a a d e e e e))
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
-- Example in Haskell:
--
-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode :: (Eq a) => [a] -> [(Int, a)]
encode list = map (\elem@(x:xs) -> (length elem, x)) (pack list)

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' list = [(length x, head x) | x <- pack list]