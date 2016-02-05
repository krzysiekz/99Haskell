module Lists.Problem1 where

-- (*) Find the last element of a list.
--
-- Example in Haskell:
-- Prelude> myLast [1,2,3,4]
-- 4
-- Prelude> myLast ['x','y','z']
-- 'z'

lastElementRecursion :: [a] -> a
lastElementRecursion [] = error "Empty list"
lastElementRecursion [x] = x
lastElementRecursion (x:xs) = lastElementRecursion xs

lastElementReverse :: [a] -> a
lastElementReverse [] = error "Empty list"
lastElementReverse list = head $ reverse list

lastElementFold :: [a] -> a
lastElementFold [] = error "Empty list"
lastElementFold list = head $ foldl (\acc e -> e:acc) [] list

lastElementPrelude :: [a] -> a
lastElementPrelude = last