module Lists.Problem16 where

-- (**) Drop every N'th element from a list.
--
-- Example:
--
-- * (drop '(a b c d e f g h i k) 3)
-- (A B D E G H K)
-- Example in Haskell:
--
-- *Main> dropEvery "abcdefghik" 3
-- "abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery list c = map fst $ filter (\(_,b) -> b `mod` c /= 0) $ zip list [1..]

dropEvery' :: [a] -> Int -> [a]
dropEvery' list c = dropEvery'' list c 1
    where dropEvery'' [] _ _ = []
          dropEvery'' (x:xs) c i
            | i `mod` c /= 0 = x: dropEvery'' xs c (i+1)
            | otherwise =  dropEvery'' xs c (i+1)

