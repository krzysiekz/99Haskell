module Arithmetic.Problem36 where

-- (**) Determine the prime factors of a given positive integer.
--
-- Construct a list containing the prime factors and their multiplicity.
--
-- Example:
--
-- * (prime-factors-mult 315)
-- ((3 2) (5 1) (7 1))
-- Example in Haskell:
--
-- *Main> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]

import Arithmetic.Problem35
import Data.List

prime_factors_mult :: Int -> [(Int,Int)]
prime_factors_mult = map (\e -> (head e, length e)) . group . primeFactors