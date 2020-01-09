-- | Priority search queue
module PrioritySearchQueue (
    PSQ, empty, singleton, insert, extract
    ) where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | A map in which the values are priorities, with an 'extract' operation
-- returning the lowest priority.
data PSQ k p  = PSQ (Map k p) (Map p (Set k))
    deriving Show

-- | Empty priority search queue.
empty :: PSQ k p
empty = PSQ Map.empty Map.empty

-- | A key and its priority.
singleton :: (Ord p, Ord k) => k -> p -> PSQ k p
singleton k p = PSQ (Map.singleton k p) (Map.singleton p (Set.singleton k))

-- | Insert a new key and priority in the queue.  If the key is already in
-- the queue, the associated priority is replaced with the supplied value.
insert :: (Ord k, Ord p) => k -> p -> PSQ k p -> PSQ k p
insert k p pq@(PSQ priority queue) =
    case Map.lookup k priority of
        Nothing -> PSQ (Map.insert k p priority) queue'
        Just p'
          | p' <= p -> pq
          | otherwise ->
            PSQ (Map.insert k p priority) (removeEntry k p' queue')
  where
    queue' = Map.insertWith Set.union p (Set.singleton k) queue

removeEntry :: (Ord k, Ord p) => k -> p -> Map p (Set k) -> Map p (Set k)
removeEntry k p m
    | Set.null entries = Map.delete p m
    | otherwise = Map.insert p entries m
  where
    entries = Set.delete k (m!p)

-- | Extract the key with lowest priority from the queue.
extract :: (Ord k, Ord p) => PSQ k p -> Maybe (k, p, PSQ k p)
extract (PSQ priority queue) = do
    ((p, ks), _) <- Map.minViewWithKey queue
    (k, _) <- Set.minView ks
    Just (k, p, PSQ (Map.delete k priority) (removeEntry k p queue))
