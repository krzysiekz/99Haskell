module Lists.Problem21 where

-- Insert an element at a given position into a list.
--
-- Example:
--
-- * (insert-at 'alfa '(a b c d) 2)
-- (A ALFA B C D)
-- Example in Haskell:
--
-- P21> insertAt 'X' "abcd" 2
-- "aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt c (x:xs) index
    | index == 1 = c : x : insertAt c xs (index-1)
    | otherwise = x : insertAt c xs (index-1)

insertAt' :: a -> [a] -> Int -> [a]
insertAt' c list index
    | index <= length list && index > 0 = first ++ [c] ++ second
    | otherwise = error "Wrong index"
        where (first,second) = splitAt (index-1) list

insertAt'' :: a -> [a] -> Int -> [a]
insertAt'' e list index
 | index <= length list && index > 0 = take (index-1) list ++ [e] ++ drop (index-1) list
 | otherwise = error "Wrong index"