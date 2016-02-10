module Lists.Problem15 where

-- (**) Replicate the elements of a list a given number of times.
--
-- Example:
--
-- * (repli '(a b c) 3)
-- (A A A B B B C C C)
-- Example in Haskell:
--
-- > repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) c = replicate c x ++ repli xs c

repli' :: [a] -> Int -> [a]
repli' xs c = foldr ((++) . replicate c) [] xs