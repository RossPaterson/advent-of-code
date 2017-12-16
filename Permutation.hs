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
import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | A permutation of the integers that is the identity on all but a finite
-- subset.
data Permutation = Identity | Permutation (Array Int Int)

instance Eq Permutation where
    p1 == p2 = cycles p1 == cycles p2

instance Ord Permutation where
    compare p1 p2 = compare (cycles p1) (cycles p2)

instance Show Permutation where
    showsPrec d p = case cycles p of
        [] -> showString "mempty"
        [c] -> showParen (d > 10) $ showCycle c
        c:cs -> showParen (d > 6) $
            showCycle c .
            foldr (.) id [showString " <> " . showCycle c' | c' <- cs]
     where
       showCycle c = showString "cyclic " . showsPrec 11 c

instance Monoid Permutation where
    mempty = Identity
    mappend = composePerm

apply :: Permutation -> Int -> Int
apply (Permutation arr) i | inRange (bounds arr) i = arr!i
apply _ i = i

composePerm :: Permutation -> Permutation -> Permutation
composePerm Identity p2 = p2
composePerm p1 Identity = p1
composePerm p1@(Permutation arr1) p2@(Permutation arr2) =
    Permutation (listArray bds [apply p1 (apply p2 i) | i <- range bds])
  where
    (lo1, hi1) = bounds arr1
    (lo2, hi2) = bounds arr2
    bds = (min lo1 lo2, max hi1 hi2)

-- | The inverse of a permutation
invert :: Permutation -> Permutation
invert Identity = Identity
invert p@(Permutation arr) =
    Permutation (array (bounds arr) [(j, i) | (i, j) <- assocs arr])

-- | A cyclic permutation mapping each element of the list to the next,
-- and wrapping around at the end.
cyclic :: [Int] -> Permutation
cyclic vs
  | trivial unique_vs = Identity
  | otherwise =
        Permutation (listArray bds [Map.findWithDefault i i m | i <- range bds])
  where
    unique_vs = catMaybes (snd (mapAccumL unique Set.empty vs))
    unique seen v
      | Set.member v seen = (seen, Nothing)
      | otherwise = (Set.insert v seen, Just v)
    rotate [] = []
    rotate (x:xs) = xs ++ [x]
    bds = (minimum unique_vs, maximum unique_vs)
    m = Map.fromList (zip unique_vs (rotate unique_vs))

-- | Non-unit cycles of the permutation
cycles :: Permutation -> [[Int]]
cycles Identity = []
cycles (Permutation arr) = getCycles (Set.fromList (indices arr))
  where
    getCycles left = case Set.minView left of
        Nothing -> []
        Just (i, _)
          | trivial i_cycle -> others
          | otherwise -> i_cycle:others
          where
            i_cycle = getCycle i Set.empty
            others = getCycles (Set.difference left (Set.fromList i_cycle))
    getCycle i seen
      | Set.member i seen = []
      | otherwise = i:getCycle (arr!i) (Set.insert i seen)

-- | The smallest k > 0 such that p^k = id
order :: Permutation -> Int
order p = foldr lcm 1 (map length (cycles p))

trivial :: [a] -> Bool
trivial [] = True
trivial [_] = True
trivial _ = False

-- Basic permutations

swap :: Int -> Int -> Permutation
swap i j = cyclic [i, j]

-- | swap [i..j) with [j..k)
swapRanges :: Int -> Int -> Int -> Permutation
swapRanges i j k = Permutation (listArray (i, k-1) ([j..k-1] ++ [i..j-1]))
