module Lists.Problem25 where

-- Generate a random permutation of the elements of a list.
--
-- Example:
--
-- * (rnd-permu '(a b c d e f))
-- (B A D C E F)
-- Example in Haskell:
--
-- Prelude System.Random>rnd_permu "abcdef"
-- Prelude System.Random>"badcef"

import System.Random

rndPermu :: (RandomGen g, Random a, Eq a) => g -> [a] -> [a]
rndPermu _ [] = []
rndPermu gen list = element : rndPermu newGen newList
        where (randomNumber,newGen) = randomR (0,length list-1) gen
              element = list!!randomNumber
              newList = filter (/=element) list

