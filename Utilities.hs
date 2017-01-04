module Utilities where

import Data.List
import Data.Ord
import qualified Data.Set as Set

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..maxBound]

-- repeatedly apply the function until it doesn't produce a new value
whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = maybe x (whileJust f) (f x)

-- select all the elements of xs that have the least value of f
-- (f is evaluated once for each element of the list.)
leastBy :: (Ord v) => (a -> v) -> [a] -> [a]
leastBy f = map fst . head . groupBy same . sortBy (comparing snd) . map add_f
  where
    add_f x = (x, f x)
    same (_, fx) (_, fy) = fx == fy

-- sort and eliminate repetitions
-- O(n log(m)) where m is the number of unique elements
fast_nub :: Ord a => [a] -> [a]
fast_nub = Set.toList . Set.fromList
