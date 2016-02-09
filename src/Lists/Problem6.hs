module Lists.Problem6 where

-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
--
-- Example in Haskell:
--
-- *Main> isPalindrome [1,2,3]
-- False
-- *Main> isPalindrome "madamimadam"
-- True
-- *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == reverse list

isPalindromeFold :: (Eq a) => [a] -> Bool
isPalindromeFold list = foldl (\_ (a,b) -> (a==b)) True $ zip list $ reverse list

isPalindromeRecursion :: (Eq a) => [a] -> Bool
isPalindromeRecursion [] = True
isPalindromeRecursion [_] = True
isPalindromeRecursion list@(x:xs) = x == last xs && isPalindromeRecursion (init xs)
