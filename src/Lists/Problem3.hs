module Lists.Problem3 where

-- (*) Find the K'th element of a list. The first element in the list is number 1.
--
-- Example:
-- * (element-at '(a b c d e) 3)
-- c
-- Example in Haskell:
--
-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'

preludeElementAt :: [a] -> Int -> a
preludeElementAt list index
    | length list < index = error "List is not big enough"
    | otherwise = list!!(index-1)

recursionElementAt :: [a] -> Int -> a
recursionElementAt (x:xs) 1 = x
recursionElementAt list@(x:xs) index
    | length list < index = error "List is not big enough"
    | otherwise = recursionElementAt xs $ index-1

zipElementAt :: [a] -> Int -> a
zipElementAt list index
    | length list < index = error "List is not big enough"
    | otherwise = snd $ head $ filter (\(a,b) -> a==index) $ zipWith (\ a b -> (a,b)) [1..] list