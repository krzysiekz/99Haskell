module Lists.Problem22 where

-- Create a list containing all integers within a given range.
--
-- Example:
--
-- * (range 4 9)
-- (4 5 6 7 8 9)
-- Example in Haskell:
--
-- Prelude> range 4 9
-- [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range start end = [start..end]

range' :: Int -> Int -> [Int]
range' start end
    | start > end = error "Wrong range"
    | start == end = [start]
    | otherwise = start : range' (start+1) end