module Lists.Problem5 where

-- (*) Reverse a list.
--
-- Example in Haskell:
--
-- Prelude> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- Prelude> myReverse [1,2,3,4]
-- [4,3,2,1]

myReversePrelude :: [a] -> [a]
myReversePrelude = reverse

myReverseFold :: [a] -> [a]
myReverseFold = foldl (flip (:)) []

myReverseRecursion :: [a] -> [a]
myReverseRecursion [] = []
myReverseRecursion (x:xs) = myReverseRecursion xs ++ [x]


