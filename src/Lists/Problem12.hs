module Lists.Problem12 where

import Lists.Problem11

-- (**) Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
--
-- Example in Haskell:
--
-- P12> decodeModified
--        [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"


decodeModified :: [Result a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = x:decodeModified xs
decodeModified (Multiple c x:xs) = replicate c x ++ decodeModified xs

decodeModified' :: [Result a] -> [a]
decodeModified' = foldl (\acc el ->acc ++ convert el) []
    where convert (Single x) = [x]
          convert (Multiple c x) = replicate c x