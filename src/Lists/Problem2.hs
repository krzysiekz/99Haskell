module Lists.Problem2 where

-- (*) Find the last but one element of a list.
-- Example in Haskell:
--
-- Prelude> myButLast [1,2,3,4]
-- 3
-- Prelude> myButLast ['a'..'z']
-- 'y'

butLastElementRecursion :: [a] -> a
butLastElementRecursion [] = error "Empty list"
butLastElementRecursion [x] = error "list has only 1 element"
butLastElementRecursion (x:[y]) = x
butLastElementRecursion (x:xs) = butLastElementRecursion xs

butLastElementFold :: [a] -> a
butLastElementFold [] = error "Empty list"
butLastElementFold [x] = error "list has only 1 element"
butLastElementFold list = case fst $  foldl (\(a, b) x -> (b, Just x)) (Nothing, Nothing) list of
    Just x -> x
    Nothing -> error "Not found"

butLastElementFunction :: [a] -> a
butLastElementFunction = head . tail. reverse

butLastElementIndex :: [a] -> a
butLastElementIndex [] = error "Empty list"
butLastElementIndex [x] = error "list has only 1 element"
butLastElementIndex list = list !! ((length list) -2)

