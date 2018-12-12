module Permutation (
    Permutation,
    apply,
    invert,
    cyclic,
    cycles,
    order,
    swap,
    swapRanges,
  ) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | A permutation that is the identity on all but a finite subset.
newtype Permutation a = Permutation (Map a a)
    deriving (Eq, Ord)

-- Only non-identity entries are stored
permutation :: (Ord a) => [(a, a)] -> Permutation a
permutation = Permutation . Map.fromList . filter (uncurry (/=))

nonIdentity :: (Ord a) => Permutation a -> Set a
nonIdentity (Permutation m) = Map.keysSet m

instance (Ord a, Show a) => Show (Permutation a) where
    showsPrec d p = case cycles p of
        [] -> showString "mempty"
        [c] -> showParen (d > 10) $ showCycle c
        c:cs -> showParen (d > 6) $
            showCycle c .
            foldr (.) id [showString " <> " . showCycle c' | c' <- cs]
     where
       showCycle c = showString "cyclic " . showsPrec 11 c

instance (Ord a) => Semigroup (Permutation a) where
    (<>) = composePerm

instance (Ord a) => Monoid (Permutation a) where
    mempty = Permutation Map.empty

apply :: (Ord a) => Permutation a -> a -> a
apply (Permutation m) x = Map.findWithDefault x x m

composePerm :: (Ord a) => Permutation a -> Permutation a -> Permutation a
composePerm p1 p2 =
    permutation [(x, apply p1 (apply p2 x)) | x <- keys]
  where
    keys = Set.toList (Set.union (nonIdentity p1) (nonIdentity p2))

-- | The inverse of a permutation
invert :: (Ord a) => Permutation a -> Permutation a
invert p = Permutation $
    Map.fromList [(apply p x, x) | x <- Set.toList (nonIdentity p)]

-- | A cyclic permutation mapping each element of the list to the next,
-- and wrapping around at the end.
cyclic :: (Ord a) => [a] -> Permutation a
cyclic vs = permutation (zip unique_vs (rotate unique_vs))
  where
    unique_vs = catMaybes (snd (mapAccumL unique Set.empty vs))
    unique seen v
      | Set.member v seen = (seen, Nothing)
      | otherwise = (Set.insert v seen, Just v)
    rotate [] = []
    rotate (x:xs) = xs ++ [x]

-- | Non-unit cycles of the permutation, in increasing order of least element
cycles :: (Ord a) => Permutation a -> [[a]]
cycles p = unfoldr extractCycle (nonIdentity p)
  where
    extractCycle left = case Set.minView left of
        Nothing -> Nothing
        Just (i, _) ->
            Just (i_cycle, Set.difference left (Set.fromList i_cycle))
          where
            -- the cycle must have at least two elements
            i_cycle = i:takeWhile (/= i) (iterate (apply p) (apply p i))

-- | The smallest positive k such that p^k = id
order :: (Ord a) => Permutation a -> Int
order p = foldr lcm 1 (map length (cycles p))

-- Basic permutations

swap :: (Ord a) => a -> a -> Permutation a
swap i j = cyclic [i, j]

-- | swap [i..j) with [j..k)
swapRanges :: (Enum a, Ord a) => a -> a -> a -> Permutation a
swapRanges i j k = permutation (zip [i..pred k] ([j..pred k] ++ [i..pred j]))
