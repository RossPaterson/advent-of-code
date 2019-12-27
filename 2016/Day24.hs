module Main where

import Utilities
import Graph
import Data.List
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Input = Maze

data Pos = Pos Int Int
  deriving (Show, Eq, Ord)
type NodeName = Char
data Maze = Maze { openPos :: Set Pos, nodeLoc :: Map NodeName Pos }
  deriving Show

mkMaze :: [String] -> Maze
mkMaze ls = Maze {
    openPos = Set.fromList [Pos x y |
        (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l, c /= '#'],
    nodeLoc = Map.fromList [(c, Pos x y) |
        (y, l) <- zip [0..] ls, (x, c) <- zip [0..] l, c /= '#' && c /= '.']
    }

neighbours :: Maze -> Pos -> [Pos]
neighbours m (Pos x y) =
    [p | p <- [Pos (x-1) y, Pos (x+1) y, Pos x (y-1), Pos x (y+1)],
        Set.member p (openPos m) ]

nodes :: Maze -> [NodeName]
nodes m = Map.keys (nodeLoc m)

parse :: String -> Input
parse = mkMaze . lines

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
