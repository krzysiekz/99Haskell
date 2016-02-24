module Arithmetic.Problem31 where

-- (**) Determine whether a given integer number is prime.
--
-- Example:
--
-- * (is-prime 7)
-- T
-- Example in Haskell:
--
-- P31> isPrime 7
-- True

isPrime :: Int -> Bool
isPrime 0 = False
isPrime num = foldl (\acc e -> acc && (num `mod` e /= 0)) True [2..(num-1)]

isPrime' :: Int -> Bool
isPrime' 1 = True
isPrime' 0 = False
isPrime' num = isPrime'' num (num-1)
    where isPrime'' :: Int -> Int -> Bool
          isPrime'' _ 1 = True
          isPrime'' num current = (num `mod` current /= 0) && isPrime'' num (current-1)

isPrime''' :: Int -> Bool
isPrime''' 0 = False
isPrime''' 1 = True
isPrime''' num = filter (\e -> num `mod` e == 0) [1..num] == [1, num]

