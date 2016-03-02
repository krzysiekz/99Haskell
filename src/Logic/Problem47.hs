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

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _ = False

xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True

table2 :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table2 f = [(a, b, f a b) | a <- [True, False], b <- [True, False]]

