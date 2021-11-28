-- | Minimum priority queue, implemented with lazy skew heaps, from
-- "Fun with Binary Heap Trees" by Chris Okasaki in
-- "The Fun of Programming", pp 1-16.
module Data.MinPriorityQueue (PQ, singleton, insert, extract) where

-- | Minimum priority queue
data PQ p a = Null | Fork p a (PQ p a) (PQ p a)

instance (Ord p) => Semigroup (PQ p a) where
    (<>) = merge

instance (Ord p) => Monoid (PQ p a) where
    mempty = empty

-- | Extract the element with the least key
extract :: Ord p => PQ p a -> Maybe (p, a, PQ p a)
extract Null = Nothing
extract (Fork p x a b) = Just (p, x, merge a b)

-- | Empty priority queue
empty :: PQ p a
empty = Null

-- | Singleton priority queue
singleton :: p -> a -> PQ p a
singleton p x = Fork p x Null Null

-- | Add an item to a priority queue
insert :: Ord p => p -> a -> PQ p a -> PQ p a
insert p x a = merge (singleton p x) a

-- | Merge two priority queues
merge :: Ord p => PQ p a -> PQ p a -> PQ p a
merge a Null = a
merge Null b = b
merge a@(Fork ap ax aa ab) b@(Fork bp bx ba bb)
  | ap <= bp  = Fork ap ax ab (merge aa b)
  | otherwise = Fork bp bx bb (merge ba a)
