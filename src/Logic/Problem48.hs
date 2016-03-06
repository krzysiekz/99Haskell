module Logic.Problem48 where

-- (**) Truth tables for logical expressions (3).
--
-- Generalize problem P47 in such a way that the logical expression may contain any number of
-- logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for
-- the expression Expr, which contains the logical variables enumerated in List.
--
-- Example:
--
-- * (table (A,B,C) (A and (B or C) equ A and B or A and C))
-- true true true true
-- true true fail true
-- true fail true true
-- true fail fail true
-- fail true true true
-- fail true fail true
-- fail fail true true
-- fail fail fail true
-- Example in Haskell:
--
-- > tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- -- infixl 3 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True
--
-- -- infixl 7 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False False
-- False True  True  False
-- False True  False False
-- False False True  False
-- False False False False

import Logic.Problem47

tablen :: Int  -> ([Bool] -> Bool) -> [([Bool], Bool)]
tablen num f = map (\e -> (e, f e)) $ combinations num
    where combinations :: Int -> [[Bool]]
          combinations 1 = [[True],[False]]
          combinations num = [x:xs | x <- [True, False],
                                     xs <- combinations (num-1)]