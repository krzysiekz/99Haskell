module Lists.Problem23 where

-- Extract a given number of randomly selected elements from a list.
--
-- Example:
--
-- * (rnd-select '(a b c d e f g h) 3)
-- (E D A)
-- Example in Haskell:
--
-- Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
-- eda

import System.Random

randomElements :: (RandomGen g, Random a, Eq a) => g -> [a] -> Int -> [a]
randomElements _ _ 0 = []
randomElements gen list amount
    | amount > length list = error "List to small"
    | otherwise = element : randomElements newGen newList (amount-1)
        where (randomNumber,newGen) = randomR (1,length list) gen
              element = list!!(randomNumber-1)
              newList = filter (/=element) list