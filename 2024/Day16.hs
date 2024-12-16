module Main where

import Graph
import Geometry
import Utilities
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Maze
type Maze = (Position, Position, Set Position)

parse :: String -> Input
parse s = (start, finish, points)
  where
    start = head [p | (p, c) <- pcs, c == 'S']
    finish = head [p | (p, c) <- pcs, c == 'E']
    points = Set.fromList [p | (p, c) <- pcs, c /= '#']
    pcs = readGrid s

-- Part One

-- Compass points

data Direction = N | E | S | W
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

oneStep :: Direction -> Position
oneStep N = Position 0 (-1)
oneStep E = Position 1 0
oneStep S = Position 0 1
oneStep W = Position (-1) 0

opposite :: Direction -> Direction
opposite d = toEnum ((fromEnum d + 2) `mod` 4)

type State = (Position, Direction)

-- Possible moves in the maze:
-- step in the current direction (costs 1), 90-degree turn (costs 1000).
moves :: Set Position -> State -> [(Int, State)]
moves points (p, d) =
    [(1, (p', d)) | let p' = p .+. oneStep d, Set.member p' points] ++
    [(1000, (p, d')) | d' <- [predWrap d, succWrap d]]

solve1 :: Input -> Int
solve1 (start, finish, points) =
    head [cost |
        (cost, p) <- shortestPaths (moves points) [(start, E)],
        fst p == finish]

testInput1 :: String
testInput1 = "\
    \###############\n\
    \#.......#....E#\n\
    \#.#.###.#.###.#\n\
    \#.....#.#...#.#\n\
    \#.###.#####.#.#\n\
    \#.#.#.......#.#\n\
    \#.#.#####.###.#\n\
    \#...........#.#\n\
    \###.#.#####.#.#\n\
    \#...#.....#.#.#\n\
    \#.#.#.###.#.#.#\n\
    \#.....#...#.#.#\n\
    \#.###.#.#.#.#.#\n\
    \#S..#.....#...#\n\
    \###############\n\
    \"

testInput2 :: String
testInput2 = "\
    \#################\n\
    \#...#...#...#..E#\n\
    \#.#.#.#.#.#.#.#.#\n\
    \#.#.#.#...#...#.#\n\
    \#.#.#.#.###.#.#.#\n\
    \#...#.#.#.....#.#\n\
    \#.#.#.#.#.#####.#\n\
    \#.#...#.#.#.....#\n\
    \#.#.#####.#.###.#\n\
    \#.#.#.......#...#\n\
    \#.#.###.#####.###\n\
    \#.#.#...#.....#.#\n\
    \#.#.#.#####.###.#\n\
    \#.#.#.........#.#\n\
    \#.#.#.#########.#\n\
    \#S#.............#\n\
    \#################\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 7036), (testInput2, 11048)]

-- Part Two

-- shortest distance from each state to the finish
shortest :: Set Position -> Position -> Map State Int
shortest points finish =
    Map.fromList [((p, opposite d), len) |
        (len, (p, d)) <- shortestPaths (moves points) finishes]
  where
    finishes = [(finish, d) | d <- allValues]

-- moves that are on an optimal path to the finish
optimalMoves :: Map State Int -> State -> [State]
optimalMoves cost = optimal_moves
  where
    optimal_moves s = [s' |
        cost_s <- maybeToList (Map.lookup s cost),
        (len, s') <- moves points s,
        cost_s' <- maybeToList (Map.lookup s' cost),
        cost_s == cost_s' + len]
    points = Set.map fst (Map.keysSet cost)

solve2 :: Input -> Int
solve2 (start, finish, points) =
    Set.size $ Set.fromList $ map fst $ concat $
        bfs (optimalMoves (shortest points finish)) [(start, E)]

tests2 :: [(String, Int)]
tests2 = [(testInput1, 45), (testInput2, 64)]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
