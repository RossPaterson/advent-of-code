-- | Matching in a binary relation, or equivalently a bipartite graph.
module Matching (uniquePerfectMatching) where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- | Match each key of the map with one element of the corresponding set,
-- such that each element of the sets is paired with exaxtly one key.
-- The function assumes that a unique such matching exists, and will
-- fail otherwise.
uniquePerfectMatching :: (Ord a, Ord b) => Map a (Set b) -> Map a b
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
