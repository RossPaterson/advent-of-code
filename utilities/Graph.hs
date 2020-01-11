-- | Operations on graphs represented by a neighbours function.
module Graph (
    -- * Path finding
    bfs, shortestPaths, shortestPathLF, shortestPathDF,
    -- * Connected components
    component, components
    ) where

import qualified MinPriorityQueue as PQ
import qualified PrioritySearchQueue as PSQ

import Data.List
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

-- | All nodes reachable from the start point, with shortest distances,
-- assuming all edge costs are positive.
shortestPaths :: Ord a => (a -> [(Int, a)]) -> a -> [(Int, a)]
shortestPaths adjacent start = dijkstra Set.empty (PSQ.singleton start 0)
  where
    dijkstra done psq = case PSQ.extract psq of
        Nothing -> []
        Just (n, dist_n, psq') ->
            let
                done' = Set.insert n done
                children = [(dist_n + d, child) |
                    (d, child) <- adjacent n, not (Set.member child done')]
            in (dist_n, n):dijkstra done' (foldr adjust psq' children)
    adjust (p, n) psq = PSQ.insert n p psq

-- | Shortest distance to a node satisfying the predicate, using
-- least-first search.  All edge costs must be positive.
--
-- prop> shortestPathLF p next = fst . head . filter (p . snd) . shortestPaths next
shortestPathLF :: (a -> Bool) -> (a -> [(Int, a)]) -> a -> Int
shortestPathLF succeeded adjacent = search . PQ.singleton 0
  where
    search pq = case PQ.extract pq of
        Nothing -> error "no solution"
        Just (dist_n, n, pq')
          | succeeded n -> dist_n
          | otherwise -> search (foldr add pq' (adjacent n))
          where
            add (d, child) = PQ.insert (dist_n + d) child

-- | Shortest distance to a node satisfying the predicate, using
-- depth-first search with pruning.  All edge costs must be positive.
--
-- prop> shortestPathDF p next = fst . head . filter (p . snd) . shortestPaths next
--
-- provided 'shortestPathDF' terminates.
shortestPathDF :: (a -> Bool) -> (a -> [(Int, a)]) -> a -> Int
shortestPathDF succeeded adjacent start = search 0 start maxBound
  where
    -- assume: dist_n <= best
    search dist_n n best
      | dist_n >= best = best::Int
      | succeeded n = dist_n
      | otherwise = foldr search_child best (adjacent n)
      where
        search_child (d, child) = search (dist_n + d) child

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
