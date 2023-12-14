-- | A sequence that repeats from some point.
module Data.CyclicList (
    CyclicList,
    iterate, elementAt, foldMapTake,
    prefix, repetend
    ) where

import Data.Foldable
import qualified Data.Map as Map
import Data.Semigroup
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Prelude hiding (iterate)

-- | A list of the form @xs '<>' 'cycle' ys@ where @xs@ and @ys@
-- are finite lists and @ys@ is non-empty.
data CyclicList a =
    CyclicList { front :: !(Seq a), recurring :: !(Seq a) }
  deriving Show

-- | @'prefix' xs@ is the minimal non-repeating part at the front of @xs@.
prefix :: CyclicList a -> [a]
prefix (CyclicList f _) = toList f

-- | @'repetend' xs@ is the minimal repeating part of @xs@.
repetend :: CyclicList a -> [a]
repetend (CyclicList _ r) = toList r

instance Functor CyclicList where
    fmap f (CyclicList fr re) = CyclicList (fmap f fr) (fmap f re)

instance Foldable CyclicList where
    foldr f _ (CyclicList fr re) = foldr f rest fr
      where
        rest = foldr f rest re

-- | @'iterate' f x@ is an infinite list of repeated applications of @f@
-- to @x@, provided an earlier value is repeated at some point.
-- If no repetition occurs, the computation does not terminate.
iterate :: Ord a => (a -> a) -> a -> CyclicList a
iterate f = loop Seq.empty Map.empty
  where
    loop pre position x = case Map.lookup x position of
        Nothing ->
            loop (pre |> x) (Map.insert x (Seq.length pre) position) (f x)
        Just pos -> CyclicList fr re
          where
            (fr, re) = Seq.splitAt pos pre

-- | @'elementAt' i xs@ is the element of @xs@ at position @i@
-- (counting from zero).
elementAt :: Int -> CyclicList a -> a
elementAt n (CyclicList fr re)
  | n <= Seq.length fr = Seq.index fr n
  | otherwise = Seq.index re ((n - Seq.length fr) `mod` Seq.length re)

-- | @'foldMapTake' f n xs@ applies @f@ to the first @n@ elements of @xs@
-- and combines the results:
--
-- prop> foldMapTake f n = foldMap f . take n
--
-- but may be significantly faster for some monoids.
foldMapTake :: Monoid m => (a -> m) -> Int -> CyclicList a -> m
foldMapTake f n (CyclicList fr re)
  | n <= Seq.length fr = foldMap f (Seq.take n fr)
  | q == 0 = foldMap f fr <> foldMap f (Seq.take r re)
  | otherwise =
    foldMap f fr <> stimes q (foldMap f re) <> foldMap f (Seq.take r re)
  where
    (q, r) = (n - Seq.length fr) `divMod` Seq.length re
