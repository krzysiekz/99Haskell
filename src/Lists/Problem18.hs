module Lists.Problem18 where

-- (**) Extract a slice from a list.
--
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of
-- the original list (both limits included). Start counting the elements with 1.
--
-- Example:
--
-- * (slice '(a b c d e f g h i k) 3 7)
-- (C D E F G)
-- Example in Haskell:
--
-- *Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

slice :: [a] -> Int -> Int -> [a]
slice list start end = map fst $ filter (\(_,b) -> b >= start && b <= end) $ zip list [1..]

slice' :: [a] -> Int -> Int -> [a]
slice' list start end = slice'' list start end 1
    where slice'' [] _ _ _ = []
          slice'' (x:xs) start end i
            | i >= start && i<= end = x: slice'' xs start end (i+1)
            | otherwise = slice'' xs start end (i+1)