module Lists.Problem11 where

import Lists.Problem9
import Lists.Problem10

-- (*) Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no duplicates it
-- is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
--
-- Example:
--
-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:
--
-- P11> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data Result a = Multiple Int a | Single a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Result a]
encodeModified list = map (\elem@(x:xs) ->
    if length elem == 1 then Single x else Multiple (length elem) x) (pack list)

encodeModified' :: (Eq a) => [a] -> [Result a]
encodeModified' list = [if length x == 1 then Single (head x) else Multiple (length x) (head x) | x <- pack list]

encodeModified'' :: (Eq a) => [a] -> [Result a]
encodeModified''= map convert .encode
    where convert (1, x) = Single x
          convert (c, x) = Multiple c x

