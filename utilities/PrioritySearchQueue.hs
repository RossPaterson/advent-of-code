-- | Priority search queue
module PrioritySearchQueue (
    PSQ, empty, singleton, insert, extract
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | A map in which the values are priorities, with an 'extract' operation
-- returning the lowest priority.
data PSQ k p  = PSQ (Map k p) (Set (p, k))
    deriving Show

-- | Empty priority search queue.
empty :: PSQ k p
empty = PSQ Map.empty Set.empty

-- | A key and its priority.
singleton :: (Ord p, Ord k) => k -> p -> PSQ k p
singleton k p = PSQ (Map.singleton k p) (Set.singleton (p, k))

-- | Insert a new key and priority into the queue.  If the key is already
-- in the queue with a greater priority, the associated priority is
-- replaced with the supplied value.
insert :: (Ord k, Ord p) => k -> p -> PSQ k p -> PSQ k p
insert k p pq@(PSQ priority queue) =
    case Map.lookup k priority of
        Nothing ->
            PSQ (Map.insert k p priority) (Set.insert (p, k) queue)
        Just p'
          | p' <= p -> pq
          | otherwise ->
            PSQ (Map.insert k p priority)
                (Set.insert (p, k) (Set.delete (p', k) queue))

-- | Extract the key with lowest priority from the queue.
extract :: (Ord k, Ord p) => PSQ k p -> Maybe (k, p, PSQ k p)
extract (PSQ priority queue) = do
    ((p, k), queue') <- Set.minView queue
    Just (k, p, PSQ (Map.delete k priority) queue')
