module Utilities where

import Data.List
import Data.Ord
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..maxBound]

-- like succ, but wraps at maxBound
succWrap :: (Eq a, Bounded a, Enum a) => a -> a
succWrap v
  | v == maxBound = minBound
  | otherwise = succ v

-- like pred, but wraps at minBound
predWrap :: (Eq a, Bounded a, Enum a) => a -> a
predWrap v
  | v == minBound = maxBound
  | otherwise = pred v

-- takes n xs partitions xs into groups of size n, with the possible
-- exception of the last one, which is non-empty.
takes :: Int -> [a] -> [[a]]
takes n = takeWhile (not . null) . map (take n) . iterate (drop n)

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

-- repeatedly apply the function until it doesn't produce a new value,
-- collecting all the values
iterateWhileJust :: (a -> Maybe a) -> a -> [a]
iterateWhileJust f x = x:maybe [] (iterateWhileJust f) (f x)

-- repeatedly apply the function until it produces a Left value
whileRight :: (a -> Either b a) -> a -> b
whileRight f x = either id (whileRight f) (f x)

-- repeatedly apply the function until it produces a Left value,
-- collecting all the Right values
iterateWhileRight :: (a -> Either b a) -> a -> [a]
iterateWhileRight f x = x : either (const []) (iterateWhileRight f) (f x)

-- apply a function n times
times :: Int -> (a -> a) -> a -> a
times n f = compose (replicate n f)

-- composition of a list of functions
compose :: [a -> a] -> a -> a
compose fs x = foldr id x fs

-- mtimes k p = mconcat (replicate k) p, but with O(log k) operations
mtimes :: Monoid a => Int -> a -> a
mtimes k p
  | k == 0 = mempty
  | otherwise = stimes k p

-- ways of picking one element from a list
pick :: [a] -> [(a, [a])]
pick xs = [(x, front ++ back) | (front, x:back) <- zip (inits xs) (tails xs)]

-- possible choices of n elements from a list
choose :: Int -> [a] -> [([a], [a])]
choose 0 xs = [([], xs)]
choose n [] = []
choose n (x:xs) =
    [(x:ys, rest) | (ys, rest) <- choose (n-1) xs] ++
    [(ys, x:rest) | (ys, rest) <- choose n xs]

-- possible choices of between m and n items (m <= n)
chooseBetween :: Int -> Int -> [a] -> [[a]]
chooseBetween m n [] = [[] | m == 0]
chooseBetween m n (x:xs) =
    [x:ys | n > 0,
        ys <- chooseBetween (max 0 (m-1)) (n-1) xs] ++ chooseBetween m n xs

-- select all the elements of xs that have the least value of f
-- (f is evaluated once for each element of the list.)
leastBy :: (Ord v) => (a -> v) -> [a] -> [a]
leastBy f =
    map fst . head . groupBy (same snd) . sortBy (comparing snd) . map add_f
  where
    add_f x = (x, f x)

-- Equal results under f (useful for groupBy)
same :: (Eq b) => (a -> b) -> a -> a -> Bool
same f x y = f x == f y

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

-- Run a function on a number of test inputs with expected outputs
-- and report any mismatches.
failures :: (Show a, Show b, Eq b) =>
    String -> (a -> b) -> [(a, b)] -> [String]
failures fname f xys =
    [fname ++ " " ++ showsPrec 11 x "" ++ " = " ++ show (f x) ++
        " (expected " ++ show y ++ ")" | (x, y) <- xys, f x /= y]
