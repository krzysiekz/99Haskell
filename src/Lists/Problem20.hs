module Lists.Problem20 where

-- (*) Remove the K'th element from a list.
--
-- Example in Prolog:
--
-- ?- remove_at(X,[a,b,c,d],2,R).
-- X = b
-- R = [a,c,d]
-- Example in Lisp:
--
-- * (remove-at '(a b c d) 2)
-- (A C D)
-- (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)
--
-- Example in Haskell:
--
-- *Main> removeAt 2 "abcd"
-- ('b',"acd")

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (x:xs) index
    | index == 1 = removeAt xs (index-1)
    | otherwise = x : removeAt xs (index-1)

removeAt' :: [a] -> Int -> [a]
removeAt' list index = take (index-1) list ++ drop index list

removeAt'' :: [a] -> Int -> (a,[a])
removeAt'' list index = (list!!(index-1), take (index-1) list ++ drop index list)

removeAt''' :: [a] -> Int -> (a,[a])
removeAt''' list index = (last head, init head ++ tail)
    where (head, tail) = splitAt index list
