module Lists.Problem19 where

-- (**) Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- Examples:
--
-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)
--
-- * (rotate '(a b c d e f g h) -2)
-- (G H A B C D E F)
-- Examples in Haskell:
--
-- *Main> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
--
-- *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"

rotate :: [a] -> Int -> [a]
rotate list num
    | num >= 0 = drop num list ++ take num list
    | otherwise = drop size list ++ take size list
    where size = length list + num

rotate' :: [a] -> Int -> [a]
rotate' list 0 = list
rotate' list num
    | num >= 0 = rotate' (tail list ++ [head list]) (num-1)
    | otherwise = rotate' (last list : init list) (num+1)

