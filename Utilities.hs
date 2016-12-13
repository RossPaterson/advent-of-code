module Utilities where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

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

-- repeatedly apply the function until it doesn't produce a new value
whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = maybe x (whileJust f) (f x)

-- possible choices of n elements from a list
choose :: Int -> [a] -> [([a], [a])]
choose 0 xs = [([], xs)]
choose n [] = []
choose n (x:xs) =
    [(x:ys, rest) | (ys, rest) <- choose (n-1) xs] ++
    [(ys, x:rest) | (ys, rest) <- choose n xs]

-- eliminate eadjacent repetitions
uniq :: Eq a => [a] -> [a]
uniq = map head . group

-- breadth-first search
-- bfs f xs!!k contains all the unique values reachable from xs via f
-- in k steps and no fewer.
bfs :: Ord a => (a -> [a]) -> [a] -> [[a]]
bfs f = map fst . iterate step . new_level Set.empty
  where
    step (xs, seen) = new_level seen (concatMap f xs)
    new_level seen = foldr add ([], seen)
    add x (xs, seen)
      | Set.member x seen = (xs, seen)
      | otherwise = (x:xs, Set.insert x seen)
