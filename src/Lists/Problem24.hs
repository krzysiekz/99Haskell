module Lists.Problem24 where

-- Lotto: Draw N different random numbers from the set 1..M.
--
-- Example:
--
-- * (rnd-select 6 49)
-- (23 1 17 33 21 37)
-- Example in Haskell:
--
-- Prelude System.Random>diff_select 6 49
-- Prelude System.Random>[23,1,17,33,21,37]

import System.Random
import Data.List

diffSelect :: (RandomGen g) => g -> Int -> Int -> [Int]
diffSelect gen counter num = diffSelect' gen counter num []
    where diffSelect' _ 0 _ result = result
          diffSelect' gen counter num result
            | counter > num = error "Range to small"
            | generated `elem` result = diffSelect' newGen counter num result
            | otherwise = diffSelect' newGen (counter-1) num (generated:result)
                where (generated, newGen) = randomR (1, num) gen

diffSelect'' :: (RandomGen g) => g -> Int -> Int -> [Int]
diffSelect'' gen counter num = take counter $ nub $ randomRs (1, num) gen