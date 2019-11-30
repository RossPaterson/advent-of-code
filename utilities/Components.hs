-- | Components in a bidirectional graph, represented by a neighbours function.
module Components where

import Utilities
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

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
