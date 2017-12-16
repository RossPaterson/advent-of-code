module Permutation (
    Permutation,
    apply,
    invert,
    swap,
    swapRanges,
  ) where

import Control.Applicative
import Data.Array
import Data.Char
import Data.Monoid

-- | A permutation of the integers that is the identity on all but a finite
-- subset.
data Permutation = Identity | Permutation (Array Int Int)
    deriving Show

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

invert :: Permutation -> Permutation
invert Identity = Identity
invert p@(Permutation arr) =
    Permutation (array (bounds arr) [(j, i) | (i, j) <- assocs arr])

-- Basic permutations

swap :: Int -> Int -> Permutation
swap i j
  | i == j = Identity
  | otherwise = Permutation (listArray (lo, hi) (hi:[lo+1..hi-1] ++ [lo]))
  where
    lo = min i j
    hi = max i j

-- | swap [i..j) with [j..k)
swapRanges :: Int -> Int -> Int -> Permutation
swapRanges i j k = Permutation (listArray (i, k-1) ([j..k-1] ++ [i..j-1]))
