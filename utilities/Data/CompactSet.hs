-- | Compact representation of a finite set of natural numbers.
module Data.CompactSet (
    -- * Set type
    Set,
    -- * Construction
    empty, singleton, insert, fromList,
    -- * Query
    null, size, elems, member, lookupMin, lookupMax,
    -- * Deletion
    delete, deleteMin, minView, deleteMax, maxView,
    -- * Relationships
    isSubsetOf, isProperSubsetOf, disjoint,
    -- * Combinations
    union, unions, intersection, difference
    ) where

import Data.Bits
import Data.List (unfoldr)
import Prelude hiding (null)

-- | A compact representation of a finite set of natural numbers.
newtype Set a = BitSet Integer
    deriving (Eq, Ord)

instance (Enum a, Show a) => Show (Set a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (elems xs)

-- | 'union'
instance Enum a => Semigroup (Set a) where
    (<>) = union

-- | 'empty' is the identity of 'union'.
instance Enum a => Monoid (Set a) where
    mempty = empty

-- | The empty set.
empty :: Set a
empty = BitSet zeroBits

-- | A singleton set.
singleton :: Enum a => a -> Set a 
singleton i = BitSet (bit (fromEnum i))

-- | Insert an element into a set.
insert :: Enum a => a -> Set a -> Set a
insert x xs = singleton x `union` xs

-- | Delete an element from a set.
delete :: Enum a => a -> Set a -> Set a
delete x xs = difference xs (singleton x)

-- | Create a set from a list of elements.
fromList :: Enum a => [a] -> Set a
fromList = foldr insert empty

-- | Is this the empty set?
null :: Set a -> Bool
null (BitSet s) = s == zeroBits

-- | The number of elements in the set.
size :: Set a -> Int
size (BitSet s) = popCount s

-- | Is the element in the set?
member :: Enum a => a -> Set a -> Bool
member i (BitSet s) = testBit s (fromEnum i)

-- | Is the first set a subset of the second?
isSubsetOf :: Set a -> Set a -> Bool
isSubsetOf xs ys = intersection xs ys == xs

-- | Is the first set a proper subset of the second?
isProperSubsetOf :: Set a -> Set a -> Bool
isProperSubsetOf xs ys = isSubsetOf xs ys && xs /= ys

-- | Do the two sets have no members in common?
disjoint :: Set a -> Set a -> Bool
disjoint xs ys = null (intersection xs ys)

-- | The union of two sets.
union :: Set a -> Set a -> Set a
union (BitSet xs) (BitSet ys) = BitSet (xs .|. ys)

-- | The union of a list of sets.
unions :: [Set a] -> Set a
unions = foldr union empty

-- | The intersection of two sets.
intersection :: Set a -> Set a -> Set a
intersection (BitSet xs) (BitSet ys) = BitSet (xs .&. ys)

-- | The difference between two sets.
difference :: Set a -> Set a -> Set a
difference (BitSet xs) (BitSet ys) = BitSet (xs .&. complement ys)

-- | The minimal element of a set.
lookupMin :: Enum a => Set a -> Maybe a
lookupMin (BitSet xs)
  | xs == zeroBits = Nothing
  | otherwise = Just (toEnum (until (testBit xs) (+1) 0))

-- | Delete the minimal element. Returns an empty set if the set is empty
deleteMin :: Enum a => Set a -> Set a
deleteMin xs = case lookupMin xs of
    Nothing -> empty
    Just x -> delete x xs

-- | The minimal key of the set, and the set stripped of that element,
-- or 'Nothing' if passed an empty set.
minView :: Enum a => Set a -> Maybe (a, Set a)
minView xs = do
    x <- lookupMin xs
    return (x, delete x xs)

-- | The maximal element of a set.
lookupMax :: Enum a => Set a -> Maybe a
lookupMax (BitSet xs)
  | xs == zeroBits = Nothing
  | otherwise = Just (toEnum (until (\ i -> bit i > xs) (+1) 1 - 1))

-- | Delete the maximal element. Returns an empty set if the set is empty
deleteMax :: Enum a => Set a -> Set a
deleteMax xs = case lookupMax xs of
    Nothing -> empty
    Just x -> delete x xs

-- | The maximal key of the set, and the set stripped of that element,
-- or 'Nothing' if passed an empty set.
maxView :: Enum a => Set a -> Maybe (a, Set a)
maxView xs = do
    x <- lookupMax xs
    return (x, delete x xs)

-- | The elements of a set in ascending order.
elems :: Enum a => Set a -> [a]
elems = unfoldr minView
