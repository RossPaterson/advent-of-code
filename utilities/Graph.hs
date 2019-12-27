-- | Operations on graphs represented by a neighbours function.
module Graph (
    -- * Path finding
    bfs, shortestPaths,
    -- * Connected components
    component, components
    ) where

import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Breadth-first search.
-- @'bfs' f xs!!k@ contains all the unique values reachable from @xs@
-- via @f@ in @k@ steps and no fewer.  All these lists are non-empty (so
-- the whole list is finite if the number of reachable values is finite).
bfs :: Ord a => (a -> [a]) -> [a] -> [[a]]
bfs f = takeWhile (not . null) . map fst . iterate step . new_level Set.empty
  where
    step (xs, seen) = new_level seen (concatMap f xs)
    new_level seen [] = ([], seen)
    new_level seen (x:xs)
      | Set.member x seen = new_level seen xs
      | otherwise = (x:ys, seen')
      where
        (ys, seen') = new_level (Set.insert x seen) xs

data PSQ p a = PSQ (Map a p) (Map p (Set a))
    deriving Show

singletonPSQ :: (Ord p, Ord a) => p -> a -> PSQ p a
singletonPSQ p v = PSQ (Map.singleton v p) (Map.singleton p (Set.singleton v))

addPSQ :: (Ord p, Ord a) => p -> a -> PSQ p a -> PSQ p a
addPSQ p v pq@(PSQ priority queue) =
    case Map.lookup v priority of
        Nothing -> PSQ (Map.insert v p priority) queue'
        Just p'
          | p' <= p -> pq
          | otherwise ->
            PSQ (Map.insert v p priority) (removeEntry p' v queue')
  where
    queue' = Map.insertWith Set.union p (Set.singleton v) queue

removeEntry :: (Ord p, Ord a) => p -> a -> Map p (Set a) -> Map p (Set a)
removeEntry p v m
    | Set.null entries = Map.delete p m
    | otherwise = Map.insert p entries m
  where
    entries = Set.delete v (m!p)

extractPSQ :: (Ord p, Ord a) => PSQ p a -> Maybe (p, a, PSQ p a)
extractPSQ (PSQ priority queue) = do
    ((p, vs), _) <- Map.minViewWithKey queue
    (v, _) <- Set.minView vs
    Just (p, v, PSQ (Map.delete v priority) (removeEntry p v queue))

-- | reachable nodes and shortest distances
shortestPaths :: Ord a => (a -> [(Int, a)]) -> a -> [(Int, a)]
shortestPaths children start = dijkstra Set.empty (singletonPSQ 0 start)
  where
    dijkstra done psq = case extractPSQ psq of
        Nothing -> []
        Just (p, v, psq') ->
            let done' = Set.insert v done in
            (p, v):dijkstra done' (foldr adjust psq' [(p+d, c) | (d, c) <- children v, not (Set.member c done')])
    adjust (p, v) psq = addPSQ p v psq

-- | Connected component containing this node
component :: Ord a => (a -> [a]) -> a -> Set a
component neighbours n = Set.fromList (concat (bfs neighbours [n]))

-- | Connected components of the graph
components :: Ord a => (a -> [a]) -> Set a -> [Set a]
components neighbours = unfoldr extractComponent
  where
    extractComponent ns =
        fmap (extract . component neighbours) (Set.lookupMin ns)
      where
        extract s = (s, Set.difference ns s)
