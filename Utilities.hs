module Utilities where

import Data.List
import qualified Data.Set as Set

-- select all the elements of xs that have the least value of f
leastBy :: (Ord v) => (a -> v) -> [a] -> [a]
leastBy f = head . groupBy same . sortOn f
  where
    same x y = f x == f y

-- sort and eliminate repetitions
-- O(n log(m)) where m is the number of unique elements
fast_nub :: Ord a => [a] -> [a]
fast_nub = Set.toList . Set.fromList
