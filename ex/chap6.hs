import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- numUniques [1, 1, 1] will give 1



