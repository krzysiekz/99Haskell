module Lists.Problem9 where

import Data.List

-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
--
-- Example:
--
-- * (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))
-- Example in Haskell:
--
-- *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: (Eq a) => [a] -> [[a]]
pack = group

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' list@(x:xs) = filter (==x) list : pack' (filter (/=x) xs)