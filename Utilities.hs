module Utilities where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..maxBound]

-- groups n xs partitions xs into groups of size n, with the possible
-- exception of the last one, which is non-empty.
groups :: Int -> [a] -> [[a]]
groups n = takeWhile (not . null) . map (take n) . iterate (drop n)

-- combine adjacent pairs, halving the size of the list
pairWith :: (a -> a -> a) -> [a] -> [a]
pairWith f (x1:x2:xs) = f x1 x2:pairWith f xs
pairWith f xs = xs

-- unique elements of the input list, paired with their number of occurrences
frequency :: Ord a => [a] -> [(a, Int)]
frequency xs = [(head g, length g) | g <- group (sort xs)]

-- unique elements of the input list, in decreasing order of frequency
mostCommon :: Ord a => [a] -> [a]
mostCommon xs = map snd (sort [(-n, w) | (w, n) <- frequency xs])

-- repeatedly apply the function until it doesn't produce a new value
whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = maybe x (whileJust f) (f x)

-- apply a function n times
times :: Int -> (a -> a) -> a -> a
times n f x = foldr id x (replicate n f)

-- possible choices of n elements from a list
choose :: Int -> [a] -> [([a], [a])]
choose 0 xs = [([], xs)]
choose n [] = []
choose n (x:xs) =
    [(x:ys, rest) | (ys, rest) <- choose (n-1) xs] ++
    [(ys, x:rest) | (ys, rest) <- choose n xs]

-- sort and eliminate repetitions
-- O(n log(m)) where m is the number of unique elements
fast_nub :: Ord a => [a] -> [a]
fast_nub = Set.toList . Set.fromList

-- breadth-first search
-- bfs f xs!!k contains all the unique values reachable from xs via f
-- in k steps and no fewer.  All these lists are non-empty (so the
-- whole list is finite if the number of reachable values is finite).
bfs :: Ord a => (a -> [a]) -> [a] -> [[a]]
bfs f = takeWhile (not . null) . map fst . iterate step . new_level Set.empty
  where
    step (xs, seen) = new_level seen (concatMap f xs)
    new_level seen [] = ([], seen)
    new_level seen (x:xs)
      | Set.member x seen = new_level seen xs
      | otherwise = (x:ys, seen')
      where
        (ys, seen') = new_level (Set.insert x seen) xs
