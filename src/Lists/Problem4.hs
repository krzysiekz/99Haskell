module Lists.Problem4 where

-- (*) Find the number of elements of a list.
--
-- Example in Haskell:
--
-- Prelude> myLength [123, 456, 789]
-- 3
-- Prelude> myLength "Hello, world!"
-- 13

myLengthPrelude :: [a] -> Int
myLengthPrelude = length

myLengthFold :: [a] -> Int
myLengthFold = foldl (\acc _ -> acc + 1) 0

myLengthRecursion :: [a] -> Int
myLengthRecursion [] = 0
myLengthRecursion (x:xs) = 1 + myLengthRecursion xs

myLengthMap :: [a] -> Int
myLengthMap = sum . map (const 1)
