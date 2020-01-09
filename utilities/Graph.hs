-- | Operations on graphs represented by a neighbours function.
module Graph (
    -- * Path finding
    bfs, shortestPaths,
    -- * Connected components
    component, components
    ) where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified PrioritySearchQueue as PSQ

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

-- | reachable nodes and shortest distances
shortestPaths :: Ord a => (a -> [(Int, a)]) -> a -> [(Int, a)]
shortestPaths adjacent start = dijkstra Set.empty (PSQ.singleton start 0)
  where
    dijkstra done psq = case PSQ.extract psq of
        Nothing -> []
        Just (n, p, psq') ->
            let done' = Set.insert n done in
            (p, n):dijkstra done' (foldr adjust psq' [(p+d, n') | (d, n') <- adjacent n, not (Set.member n' done')])
    adjust (p, n) psq = PSQ.insert n p psq

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
