module Arithmetic.Problem39 where

-- (*) A list of prime numbers.
--
-- Given a range of integers by its lower and upper limit,
-- construct a list of all prime numbers in that range.
--
-- Example in Haskell:
--
-- P29> primesR 10 20
-- [11,13,17,19]

import Arithmetic.Problem31

primesR :: Int -> Int -> [Int]
primesR min max = filter isPrime [min..max]

primesR' :: Int -> Int -> [Int]
primesR' min max = [x | x <- [min..max], isPrime x]