module Arithmetic.Problem35 where

-- (**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
--
-- Example:
--
-- * (prime-factors 315)
-- (3 3 5 7)
-- Example in Haskell:
--
-- > primeFactors 315
-- [3, 3, 5, 7]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors x = factor : primeFactors (x `div` factor)
    where factor = head [i | i <- [2..x] , x `mod` i == 0]