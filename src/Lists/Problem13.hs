module Lists.Problem13 where

-- (**) Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create
-- the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
--
-- Example:
--
-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:
--
-- P13> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']


data Result a = Multiple Int a | Single a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Result a]
encodeDirect [] = []
encodeDirect list@(x:xs) = createResult (length se) (head se) : encodeDirect (snd sp)
    where sp = span (==x) list
          se = fst sp
          createResult :: Int -> a -> Result a
          createResult len el
            | len == 1 = Single el
            | otherwise = Multiple len el