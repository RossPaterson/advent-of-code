-- | Cellular automata
module CellularAutomaton where

import Data.Set (Set)
import qualified Data.Set as Set

-- | The next generation of a cellular automaton, with a finite set of
-- live cells in a possibly infinite space.  Dead cells with no live
-- neighbours stay dead, so the live set stays finite.
--
-- For John Conway's original Game of Life, the cells come from an
-- infinite 2-dimensional grid, the neighbours of each cell are the 8
-- adjacent cells in the grid, and the rule is:
--
-- @
-- rule :: Bool -> Int -> Bool
-- rule True n = n == 2 || n == 3
-- rule False n = n == 3
-- @
generation :: Ord a
    => (a -> Set a) -- ^ the neighbours of a cell, assumed to be symmetric
    -> (Bool -> Int -> Bool)
            -- ^ rule for whether a cell is live on the next generation,
            -- based on whether it is live and its number of live neighbours
    -> Set a -- ^ cells live in the previous generation
    -> Set a -- ^ cells live in the next generation
generation neighbours rule s = Set.filter alive candidates
  where
    candidates = Set.union s (Set.unions (map neighbours (Set.elems s)))
    alive c = rule (Set.member c s)
        (Set.size (Set.intersection s (neighbours c)))
