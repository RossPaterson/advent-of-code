-- | A sequence that is either finite or repeats from some point.
module RationalList (
    RationalList,
    finiteList, iterate, elementAt, foldMapTake
    ) where

import Data.Foldable
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Semigroup
import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Prelude hiding (iterate)

-- | A list of the form @xs '<>' 'cycle' ys@ where @xs@ and @ys@
-- are finite lists.
data RationalList a =
    RationalList { front :: !(Seq a), recurring :: !(Seq a) }
  deriving Show

instance Functor RationalList where
    fmap f (RationalList fr re) = RationalList (fmap f fr) (fmap f re)

instance Foldable RationalList where
    foldr f z (RationalList fr re) = foldr f rest fr
      where
        rest
          | Seq.null re = z
          | otherwise = foldr f rest re

-- | Representation of a finite list
finiteList :: [a] -> RationalList a
finiteList xs = RationalList (Seq.fromList xs) Seq.empty

-- | @'iterate' f x@ is an infinite list of repeated applications of @f@
-- to @x@, provided an earlier value is repeated at some point.
-- If no repetition occurs, the computation does not terminate.
iterate :: Ord a => (a -> a) -> a -> RationalList a
iterate f = loop Seq.empty Map.empty
  where
    loop prefix position x = case Map.lookup x position of
        Nothing ->
            loop (prefix |> x) (Map.insert x (Seq.length prefix) position) (f x)
        Just pos -> RationalList fr re
          where
            (fr, re) = Seq.splitAt pos prefix

-- | @'elementAt' i xs@ is the element of @xs@ at position @i@
-- (counting from zero), or 'Nothing' if @xs@ has fewer than @i@ elements.
elementAt :: Int -> RationalList a -> Maybe a
elementAt n (RationalList fr re)
  | n <= Seq.length fr = Seq.lookup n fr
  | Seq.null re = Nothing
  | otherwise = Seq.lookup ((n - Seq.length fr) `mod` Seq.length re) re

-- | @'foldMapTake' f n xs@ applies @f@ to the first @n@ elements of @xs@
-- and combines the results:
--
-- prop> foldMapTake f n = foldMap f . take n
--
-- but may be significantly faster for some monoids.
foldMapTake :: Monoid m => (a -> m) -> Int -> RationalList a -> m
foldMapTake f n (RationalList fr re)
  | n <= Seq.length fr || Seq.null re = foldMap f (Seq.take n fr)
  | q == 0 = foldMap f fr <> foldMap f (Seq.take r re)
  | otherwise =
    foldMap f fr <> stimes q (foldMap f re) <> foldMap f (Seq.take r re)
  where
    (q, r) = (n - Seq.length fr) `divMod` Seq.length re
