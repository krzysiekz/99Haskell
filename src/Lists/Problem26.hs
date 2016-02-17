module Lists.Problem26 where

-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list
--
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are
-- C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians,
-- this result may be great. But we want to really generate all the possibilities in a list.
--
-- Example:
--
-- * (combinations 3 '(a b c d e f))
-- ((A B C) (A B D) (A B E) ... )
-- Example in Haskell:
--
-- > combinations 3 "abcdef"
-- ["abc","abd","abe",...]

import Data.List

combinations :: (Ord a) => [a] -> [[a]]
combinations list = nub [sort [x,y,z] | x <- list, y <- list, z <- list, x /= y && x /= z && y /= z]

combinations' :: (Ord a) => [a] -> Int -> [[a]]
combinations' _ 0 = [[]]
combinations' list num = [first:rest' | (first:rest) <- tails list, rest' <- combinations' rest (num-1)]

combinations'' :: Int -> [a] -> [[a]]
combinations'' 0 _ = [[]]
combinations'' n xs = [ xs !! i : x | i <- [0..length xs-1]
                                  , x <- combinations'' (n-1) (drop (i+1) xs) ]