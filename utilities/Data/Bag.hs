-- | Bags (or multisets): like sets, but with multiple occurrences.
module Data.Bag (
    Bag,
    -- * Construction
    empty,
    singleton,
    fromList,
    fromCounts,
    fromSet,
    -- * Queries
    size,
    count,
    counts,
    elementSet,
    entropy,
    -- * Combinations
    union,
    unions,
    difference,
    intersection,
    -- * Maps
    map,
    concatMap,
    unionMap,
    ) where

import Prelude hiding (map, concatMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

-- | Bag or multiset of values
newtype Bag a = Bag (Map a Int)
    deriving (Eq, Ord)

instance (Enum a, Show a) => Show (Bag a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromCounts " . shows (counts xs)

-- | Empty bag
empty :: Bag a
empty = Bag Map.empty

-- | Singleton bag
singleton :: a -> Bag a
singleton x = Bag (Map.singleton x 1)

-- | Bag of elements in a list
fromList :: Ord a => [a] -> Bag a
fromList xs = Bag (Map.fromListWith (+) [(x, 1) | x <- xs])

-- | Bag of elements with number of repetitions
fromCounts :: Ord a => [(a, Int)] -> Bag a
fromCounts xns = Bag (Map.fromListWith (+) [xn | xn@(_, n) <- xns, n > 0])

-- | Bag of unique elements
fromSet :: Ord a => Set a -> Bag a
fromSet xs = Bag (Map.fromSet (const 1) xs)

-- | Total number of elements
size :: Bag a -> Int
size (Bag xs) = sum (Map.elems xs)

-- | The number of occurrences of the element in the bag
count :: Ord a => a -> Bag a -> Int
count x (Bag xs) = Map.findWithDefault 0 x xs

-- | Elements paired with number of occurrences
counts :: Bag a -> [(a, Int)]
counts (Bag xs) = Map.assocs xs

-- | Unique slements
elementSet :: Bag a -> Set a
elementSet (Bag xs) = Map.keysSet xs

-- | The entropy (base 2) of the bag interpreted as a discrete probability
-- distribution, ranging from 0 for a single value to \(\log_2 n\)
-- for no repetitions.
entropy :: Bag a -> Double
entropy (Bag xs) =
    (log n - sum [a*log a | x <- Map.elems xs, let a = fromIntegral x]/n)/log 2
  where
    n = fromIntegral (sum (Map.elems xs))

-- | Bag union:
-- @'count' a ('union' xs ys) = 'count' a xs + 'count' a ys@
union :: Ord a => Bag a -> Bag a -> Bag a
union (Bag xs) (Bag ys) = Bag (Map.unionWith (+) xs ys)

-- | Union of multiple bags
unions :: Ord a => [Bag a] -> Bag a
unions = foldr union empty

-- | Bag difference:
-- @'count' a ('difference' xs ys) = max 0 ('count' a xs - 'count' a ys@)
difference :: Ord a => Bag a -> Bag a -> Bag a
difference (Bag xs) (Bag ys) =
    Bag (Map.difference xs ys `Map.union`
        Map.filter (>0) (Map.intersectionWith (-) xs ys))

-- | Bag intersection:
-- @'count' a ('intersection' xs ys) = min ('count' a xs) ('count' a ys)@)
intersection :: Ord a => Bag a -> Bag a -> Bag a
intersection (Bag xs) (Bag ys) = Bag (Map.intersectionWith min xs ys)

-- | Bag of results from mapping each element
map :: (Ord a, Ord b) => (a -> b) -> Bag a -> Bag b
map f (Bag xs) = Bag (Map.mapKeys f xs)

-- | Bag of results from mapping each element
concatMap :: (Ord a, Ord b) => (a -> [b]) -> Bag a -> Bag b
concatMap f (Bag xs) =
    Bag (Map.fromListWith (+) [(y, n) | (x, n) <- Map.assocs xs, y <- f x])

-- | Bag of results from mapping each element
unionMap :: (Ord a, Ord b) => (a -> Bag b) -> Bag a -> Bag b
unionMap f (Bag xs) = unions [scale n (f x) | (x, n) <- Map.assocs xs]

scale :: Int -> Bag a -> Bag a
scale n (Bag xs) = Bag (Map.map (*n) xs)
