module Lists.Problem17 where

-- (*) Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- Example:
--
-- * (split '(a b c d e f g h i k) 3)
-- ( (A B C) (D E F G H I K))
-- Example in Haskell:
--
-- *Main> split "abcdefghik" 3
-- ("abc", "defghik")

split :: [a] -> Int -> ([a], [a])
split list c = (map fst first, map fst second)
    where zipped = zip list [1..]
          first = filter (\(_,b) -> b <= c) zipped
          second = filter (\(_,b) -> b > c) zipped

split' :: [a] -> Int -> ([a], [a])
split' list c = split'' list c 1 [] []
    where split'' [] _ _ first second= (first,second)
          split'' (x:xs) c i first second
            | i <= c = split'' xs c (i+1) (first ++ [x]) second
            | otherwise =  split'' xs c (i+1) first (second ++ [x])