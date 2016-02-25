module Arithmetic.Problem32 where

-- (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
--
-- Example:
--
-- * (gcd 36 63)
-- 9
-- Example in Haskell:
--
-- [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]

myGCD :: Int -> Int -> Int
myGCD x y
    | a == b = a
    | a < b = myGCD a (b - a)
    | b < a = myGCD b (a - b)
    where a = abs x
          b = abs y


myGCD' :: Int -> Int -> Int
myGCD' x y
    | b == 0 = a
    | otherwise = myGCD' b (a `mod` b)
    where a = abs x
          b = abs y