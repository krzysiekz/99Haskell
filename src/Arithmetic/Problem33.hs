module Arithmetic.Problem33 where

-- (*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
--
-- Example:
--
-- * (coprime 35 64)
-- T
-- Example in Haskell:
--
-- * coprime 35 64
-- True


import Arithmetic.Problem32

coprime :: Int -> Int -> Bool
coprime x y = 1 == myGCD' x y

coprime' :: Int -> Int -> Bool
coprime' x y = 1 == gcd x y