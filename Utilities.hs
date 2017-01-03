module Utilities where

import Data.List
import qualified Data.Set as Set

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..maxBound]

-- repeatedly apply the function until it doesn't produce a new value
whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = maybe x (whileJust f) (f x)

-- select all the elements of xs that have the least value of f
leastBy :: (Ord v) => (a -> v) -> [a] -> [a]
leastBy f = head . groupBy same . sortOn f
  where
    same x y = f x == f y

-- sort and eliminate repetitions
-- O(n log(m)) where m is the number of unique elements
fast_nub :: Ord a => [a] -> [a]
fast_nub = Set.toList . Set.fromList
