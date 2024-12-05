-- | Operations on relations and graphs
module Graph (
    -- * Relations
    Relation,
    relation,
    testRelation,
    uniquePerfectMatching,
    -- * Finite graphs
    FiniteGraph,
    tsort,
    components,
    scc,
    -- * Single-source shortest paths
    -- | Neighbours of a node are given by a function, so these operations
    -- can be used on infinite graphs.
    bfs, bfsPaths, shortestPaths,
    -- * Drawing graphs with Graphviz
    directedGV, undirectedGV,
    ) where

import qualified Data.PrioritySearchQueue as PSQ

import Data.Foldable (fold, toList)
import Data.List
import Data.Maybe
import Data.Tuple (swap)
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Relations

-- | A binary relation, or equivalently a bipartite graph
type Relation a b = Map a (Set b)

-- | Make a relation from a list of pairs
relation :: (Ord a, Ord b) => [(a, b)] -> Relation a b
relation xys =
    Map.fromListWith Set.union [(x, Set.singleton y) | (x, y) <- xys]

-- | Does the relation relate x and y?
testRelation :: (Ord a, Ord b) => Relation a b -> a -> b -> Bool
testRelation r x y = Set.member y (Map.findWithDefault Set.empty x r)

assocs :: Relation a b -> [(a, b)]
assocs r = [(x, y) | (x, ys) <- Map.assocs r, y <- Set.elems ys]

domain :: Relation a b -> Set a
domain = Map.keysSet

codomain :: Ord b => Relation a b -> Set b
codomain = fold

inverse :: (Ord a, Ord b) => Relation a b -> Relation b a
inverse = relation . map swap . assocs

-- | Match each key of the map with one element of the corresponding set,
-- such that each element of the sets is paired with exactly one key.
-- The function assumes that a unique such matching exists, and will
-- fail otherwise.
uniquePerfectMatching :: (Ord a, Ord b) => Relation a b -> Map a b
uniquePerfectMatching = Map.unions . unfoldr match

-- If the graph is non-empty, match the keys which have a unique partner,
-- and remove all paired nodes from the graph.
--
-- If a non-empty bipartite graph has a unique perfect matching, it
-- must be a subgraph of a half graph, and thus must have at least one
-- such pairing.  Moreover, the residual graph must also have a unique
-- perfect matching.
match :: (Ord a, Ord b) => Map a (Set b) -> Maybe (Map a b, Map a (Set b))
match r
  | Map.null r = Nothing
  | any Set.null r || Map.null matches || Set.size matched < Map.size matches =
    error "no unique perfect matching"
  | otherwise = Just (matches, Map.map (`Set.difference` matched) rest)
  where
    matched = Set.fromList (Map.elems matches)
    (matches, rest) = Map.mapEither isSingleton r
    isSingleton s
      | Set.size s == 1 = Left (Set.findMin s)
      | otherwise = Right s

-- Finite graphs

-- | A finite directed graph
type FiniteGraph a = Relation a a

vertices :: Ord a => FiniteGraph a -> Set a
vertices g = domain g <> codomain g

postorder :: Forest a -> [a]
postorder ts = foldr (foldTree postorderNode) [] ts

postorderNode :: a -> [[a] -> [a]] -> [a] -> [a]
postorderNode x fs = compose fs . (x:)

compose :: [a -> a] -> a -> a
compose fs x = foldr id x fs

treeToSet :: Ord a => Tree a -> Set a
treeToSet = Set.fromList . toList

dfs :: Ord a => FiniteGraph a -> [a] -> Forest a
dfs g = dfsPrune . unfoldForest (expandNode g)

expandNode :: Ord a => FiniteGraph a -> a -> (a, [a])
expandNode g n = (n, Set.elems (Map.findWithDefault Set.empty n g))

dfsPrune :: Ord a => Forest a -> Forest a
dfsPrune = snd . pruneForest Set.empty

pruneForest :: Ord a => Set a -> Forest a -> (Set a, Forest a)
pruneForest seen ns = fmap catMaybes (mapAccumL pruneTree seen ns)

pruneTree :: Ord a => Set a -> Tree a -> (Set a, Maybe (Tree a))
pruneTree seen (Node n ts)
  | Set.member n seen = (seen, Nothing)
  | otherwise = (seen', Just (Node n ts'))
  where
    (seen', ts') = pruneForest (Set.insert n seen) ts

dff :: Ord a => FiniteGraph a -> Forest a
dff g = dfs g (Set.elems (vertices g))

-- | Topological sort: given an acyclic graph, produces a list of the
-- the vertices of the graph such that each vertex precedes all of its
-- associated vertices.
tsort :: Ord a => FiniteGraph a -> [a]
tsort = reverse . postorder . dff

undirected :: Ord a => FiniteGraph a -> FiniteGraph a
undirected g = Map.unionWith Set.union g (inverse g)

-- | Connected components of the graph, ignoring the direction of edges
components :: Ord a => FiniteGraph a -> [Set a]
components = map treeToSet . dff . undirected

-- | Strongly connected components in topological order
scc :: Ord a => FiniteGraph a -> [Set a]
scc g = map treeToSet (dfs (inverse g) (tsort g))

-- Single-source shortest paths

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

-- | @'bfsPaths' f xs!!k@ contains paths @[y0, y1, ..., yk]@ such
-- that @yk@ is a member of @xs@ and every other @yi@ is a member of
-- @f y{i+1}@.  Each reachable @y0@ is represented by a single path of
-- minimal length.
bfsPaths :: Ord a => (a -> [a]) -> [a] -> [[[a]]]
bfsPaths f =
    takeWhile (not . null) . map (map (uncurry (:)) . fst) .
        iterate step . new_level Set.empty . map (flip (,) [])
  where
    step (xps, seen) = new_level seen [(x', x:p) | (x, p) <- xps, x' <- f x]
    new_level seen [] = ([], seen)
    new_level seen (xp@(x, _):xps)
      | Set.member x seen = new_level seen xps
      | otherwise = (xp:ys, seen')
      where
        (ys, seen') = new_level (Set.insert x seen) xps

-- | All nodes reachable from any of the start points, with shortest
-- distances, in increasing order of distance.  All edge costs must
-- be positive.
shortestPaths :: Ord a => (a -> [(Int, a)]) -> [a] -> [(Int, a)]
shortestPaths adjacent starts = dijkstra Set.empty start_q
  where
    start_q = foldr (\ s -> PSQ.insert s 0) PSQ.empty starts
    dijkstra done psq = case PSQ.extract psq of
        Nothing -> []
        Just (n, dist_n, psq') ->
            let
                done' = Set.insert n done
                children = [(dist_n + d, child) |
                    (d, child) <- adjacent n, not (Set.member child done')]
            in (dist_n, n):dijkstra done' (foldr adjust psq' children)
    adjust (p, n) psq = PSQ.insert n p psq

-- | Convert graph to Graphviz directed graph format for drawing.
-- Each pair of node names, representing a directed edge, has an attached
-- list of attribute-value pairs.
directedGV :: [(String, String, [(String, String)])] -> String
directedGV = graphviz "digraph" "->"

-- | Convert graph to Graphviz undirected graph format for drawing.
-- Each pair of node names, representing an undirected edge, has an
-- attached list of attribute-value pairs.
undirectedGV :: [(String, String, [(String, String)])] -> String
undirectedGV = graphviz "graph" "--"

graphviz ::
    String -> String -> [(String, String, [(String, String)])] -> String
graphviz gtype edge es =
    unlines ((gtype ++ " aoc {") : map showEdge es ++ ["}"])
  where
    showEdge (c, c', as) =
        "    " ++ c ++ " " ++ edge ++ " " ++ c' ++ showAttrs as ++ ";"
    showAttrs [] = ""
    showAttrs as =
        " [" ++ intercalate "," [n ++ "=" ++ v | (n, v) <- as] ++ "]"
