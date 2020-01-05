module Main where

import Utilities
import Cartesian
import Graph
import Data.List
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Input = Maze

type NodeName = Char
data Maze = Maze { openPos :: Set Position, nodeLoc :: Map NodeName Position }
  deriving Show

parse :: String -> Input
parse s = Maze {
    openPos = Set.fromList [p | (p, c) <- pcs, c /= '#'],
    nodeLoc = Map.fromList [(c, p) | (p, c) <- pcs, c /= '#' && c /= '.']
    }
  where
    pcs = readGrid s

-- Part One --

neighbours :: Maze -> Position -> [Position]
neighbours m (Position x y) =
    [p | p <- [Position (x-1) y, Position (x+1) y, Position x (y-1), Position x (y+1)],
        Set.member p (openPos m) ]

nodes :: Maze -> [NodeName]
nodes m = Map.keys (nodeLoc m)

-- distances between nodes within the maze
type Graph = Map (NodeName, NodeName) Int

mkGraph :: Maze -> Graph
mkGraph m =
    Map.fromList [((src, dest), dist) |
        src <- nodes m,
        (dist, ns) <- zip [0..] (bfs (neighbours m) [nodeLoc m!src]),
        n <- ns,
        dest <- maybeToList (Map.lookup n nodeOf)]
  where
    nodeOf = Map.fromList [(p, n) | (n, p) <- Map.assocs (nodeLoc m)]

startNode :: NodeName
startNode = '0'

solve1 :: Input -> Int
solve1 m = minimum (map cost (permutations other_ns))
  where
    cost path = sum [g!e | e <- zip (startNode:path) path]
    other_ns = filter (/= startNode) (nodes m)
    g = mkGraph m

testInput :: String
testInput =
    "###########\n\
    \#0.1.....2#\n\
    \#.#######.#\n\
    \#4.......3#\n\
    \###########\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 14)]

-- Part Two --

solve2 :: Input -> Int
solve2 m = minimum (map cost (permutations other_ns))
  where
    cost path = sum [g!e | e <- zip (startNode:path) (path ++ [startNode])]
    other_ns = filter (/= startNode) (nodes m)
    g = mkGraph m

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
