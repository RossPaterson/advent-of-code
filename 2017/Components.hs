-- components in a bidirectional graph, represented by a neighbours function
module Components where

import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- connected component containing this node
component :: Ord a => (a -> [a]) -> a -> Set a
component neighbours n = Set.fromList (concat (bfs neighbours [n]))

-- connected components of the graph
components :: Ord a => (a -> [a]) -> Set a -> [Set a]
components neighbours ns = case Set.minView ns of
    Nothing -> []
    Just (n, _) -> comp:components neighbours (Set.difference ns comp)
      where
        comp = component neighbours n
