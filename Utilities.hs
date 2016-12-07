module Utilities where

import Data.List

-- groups n xs partitions xs into groups of size n, with the possible
-- exception of the last one, which is non-empty.
groups :: Int -> [a] -> [[a]]
groups n = takeWhile (not . null) . map (take n) . iterate (drop n)

-- unique elements of the input list, paired with their number of occurrences
frequency :: Ord a => [a] -> [(a, Int)]
frequency xs = [(head g, length g) | g <- group (sort xs)]

-- unique elements of the input list, in decreasing order of frequency
mostCommon :: Ord a => [a] -> [a]
mostCommon xs = map snd (sort [(-n, w) | (w, n) <- frequency xs])
