module Logic.Problem47 where

-- (*) Truth tables for logical expressions (2).
--
-- Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in
-- the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.
--
-- Example:
--
-- * (table A B (A and (A or not B)))
-- true true true
-- true fail true
-- fail true fail
-- fail fail fail
-- Example in Haskell:
--
-- > table2 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False

table2 :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table2 f = [(a, b, f a b) | a <- [True, False], b <- [True, False]]

