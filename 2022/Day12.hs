module Main where

import Utilities
import Geometry
import Graph
import Data.Char
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = HeightMap
type HeightMap = (Map Position Int, Position, Position)

parse :: String -> Input
parse s = (height_map, start, end)
  where
    height_map = Map.fromList [(p, height c) | (p, c) <- grid]
    start = head [p | (p, c) <- grid, c == 'S']
    end = head [p | (p, c) <- grid, c == 'E']
    grid = readGrid s
    height 'S' = 0
    height 'E' = 25
    height c = ord c - ord 'a'

-- Part One

-- points reachable from p in one step
successors :: Map Position Int -> Position -> [Position]
successors m p =
    [p' | u <- unitVectors, let p' = p .+. u,
        h' <- maybeToList (Map.lookup p' m), h' <= h+1]
  where
    h = m ! p

solve1 :: Input -> Int
solve1 (m, s, e) =
    length $ takeWhile (notElem e) $ bfs (successors m) [s]

testInput :: String
testInput = "\
    \Sabqponm\n\
    \abcryxxl\n\
    \accszExk\n\
    \acctuvwj\n\
    \abdefghi\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 31)]

-- Part Two

-- Fast enough: search forward from each possible startpoint and take
-- the minimum of the shortest path lengths.  Note that the destination
-- may not be reachable from some start points.
-- Faster and simpler: search backward from the destination

-- points from which p is reachable in one step
predecessors :: Map Position Int -> Position -> [Position]
predecessors m p =
    [p' | u <- unitVectors, let p' = p .+. u,
        h' <- maybeToList (Map.lookup p' m), h <= h'+1]
  where
    h = m ! p

-- are any of the points at elevation 0?
finished :: Map Position Int -> [Position] -> Bool
finished m ps = or [m ! p == 0 | p <- ps]

solve2 :: Input -> Int
solve2 (m, _, e) =
    length $ takeWhile (not . finished m) $ bfs (predecessors m) [e]

tests2 :: [(String, Int)]
tests2 = [(testInput, 29)]

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
