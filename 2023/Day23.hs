module Main where

import Geometry
import Graph
import Utilities
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Tree
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

data Direction = N | E | S | W
    deriving (Bounded, Enum, Eq, Ord, Show)

-- the opposite direction
opposite :: Direction -> Direction
opposite N = S
opposite E = W
opposite S = N
opposite W = E
 
-- one position in the direction
oneStep :: Direction -> Position
oneStep N = Position 0 (-1)
oneStep E = Position 1 0
oneStep S = Position 0 1
oneStep W = Position (-1) 0

type TrailMap = Map Position Cell
data Cell = Path | Slope Direction
    deriving (Show)

type Input = TrailMap

parse :: String -> Input
parse s =
    Map.fromList [(p, cell) | (p, c) <- pcs, cell <- maybeToList (readCell c)]
  where
    pcs = readGrid s

readCell :: Char -> Maybe Cell
readCell c
  | c == '.' = Just Path
  | c == '^' = Just (Slope N)
  | c == '>' = Just (Slope E)
  | c == 'v' = Just (Slope S)
  | c == '<' = Just (Slope W)
  | otherwise = Nothing

-- Part One

-- start at the top left corner
startPos :: TrailMap -> Position
startPos m = fst (Map.findMin m)

-- finish at the bottom right corner
finishPos :: TrailMap -> Position
finishPos m = fst (Map.findMax m)

-- positions reachable from the current position in one step
nextSteps :: TrailMap -> Position -> [Position]
nextSteps m p = case m!p of
    Path -> [p' | d <- unitVectors, let p' = p .+. d, Map.member p' m]
    Slope d -> [p .+. oneStep d]

-- current position and those visited previously
type Visit = (Position, Set Position)

-- lengths of all trails from the start to the finish
trails :: TrailMap -> [Int]
trails m =
    [Set.size visited |
        (p, visited) <- toList $ iterateTree (nextVisits m) (start, Set.empty),
        p == goal]
  where
    start = startPos m
    goal = finishPos m

nextVisits :: TrailMap -> Visit -> [Visit]
nextVisits m (p, visited) =
    [(p', Set.insert p visited) |
        p' <- nextSteps m p,
        not (Set.member p' visited)]

solve1 :: Input -> Int
solve1 m = maximum (trails m)

testInput :: String
testInput = "\
    \#.#####################\n\
    \#.......#########...###\n\
    \#######.#########.#.###\n\
    \###.....#.>.>.###.#.###\n\
    \###v#####.#v#.###.#.###\n\
    \###.>...#.#.#.....#...#\n\
    \###v###.#.#.#########.#\n\
    \###...#.#.#.......#...#\n\
    \#####.#.#.#######.#.###\n\
    \#.....#.#.#.......#...#\n\
    \#.#####.#.#.#########v#\n\
    \#.#...#...#...###...>.#\n\
    \#.#.#v#######v###.###v#\n\
    \#...#.>.#...>.>.#.###.#\n\
    \#####v#.#.###v#.#.###.#\n\
    \#.....#...#...#.#.#...#\n\
    \#.#########.###.#.#.###\n\
    \#...###...#...#...#.###\n\
    \###.###.#.###v#####v###\n\
    \#...#...#.#.>.>.#.>.###\n\
    \#.###.###.#.###.#.#v###\n\
    \#.....###...###...#...#\n\
    \#####################.#\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 94)]

-- Part Two

-- treat slopes as ordinary path
nextStepsAll :: TrailMap -> Position -> [Position]
nextStepsAll m p =
    [p' | d <- unitVectors, let p' = p .+. d, Map.member p' m]

-- graph with a weight on the edge to each neighbour
type WeightedGraph a = Map a [(Int, a)]

-- Weighted graph whose nodes are start, finish and forks in the path
waypointGraph :: TrailMap -> WeightedGraph Position
waypointGraph m = Map.fromSet outEdges wps
  where
    -- edges from a waypoint to other waypoints, without passing through
    -- waypoints on the way
    outEdges p = [(n, p') |
        (n, ps) <- zip [1..] (bfs next (nextStepsAll m p)),
        p' <- ps,
        p' /= p,
        Set.member p' wps]
    -- neighbouring paths, bu don't move past a waypoint
    next p
      | Set.member p wps = []
      | otherwise = nextStepsAll m p
    wps = waypoints m

-- start, finish and forks in the path
waypoints :: TrailMap -> Set Position
waypoints m =
    Set.fromList [p | p <- Map.keys m,
        p == startPos m || p == finishPos m || length (nextStepsAll m p) > 2]

-- Convert graph to Graphviz undirected graph format for rendering
graphToGV :: Map Position [(Int, Position)] -> String
graphToGV m = undirectedGV
    [(showPos p1, showPos p2, [("label", show n)]) |
        (p1, nps) <- Map.assocs m, (n, p2) <- nps, p1 < p2]
  where
    showPos (Position x y) = "P" ++ show x ++ "_" ++ show y

-- Finding the longest path in a graph is NP-hard, so we try a
-- branch-and-bound search.

type State a = (Int, a, Set a)

-- states reachable by following an edge to a node not yet visited,
-- with larger lengths first (i.e. greedy search)
nextStates :: (Ord a) => WeightedGraph a -> State a -> [State a]
nextStates g (total, x, not_visited) =
    [(total+n, y, Set.delete y not_visited) |
        (n, y) <- sortOn fst (g!x),
        Set.member y not_visited]

-- expansion of all states reachable from the start
allStates :: (Ord a) => WeightedGraph a -> a -> Tree (State a)
allStates g start =
    iterateTree (nextStates g)
        (0, start, Set.delete start (Map.keysSet g))

-- longest path from the start to the goal in the graph
longestPath :: (Ord a) => WeightedGraph a -> a -> a -> Int
longestPath g start goal =
    maximumDF (score goal) (bound g) (allStates g start)

-- The path length only counts if the path ends at the goal.
score :: (Eq a) => a -> State a -> Int
score goal (total, x, _)
  | x == goal = total
  | otherwise = 0

-- upper bound on total lengths of extensions of the current path to the goal
-- All the remaining edges must be from x or an unvisited node to an
-- unvisited node.
bound :: (Ord a) => WeightedGraph a -> State a -> Int
bound g (total, x, not_visited) =
    total + sum [maximum (0:[n | (n, x'') <- g!x', x'' == x || Set.member x'' not_visited]) |
        x' <- Set.elems not_visited]

-- The longest trail is the longest path in the waypoint graph.
maxTrail :: TrailMap -> Int
maxTrail m = longestPath (waypointGraph m) (startPos m) (finishPos m)

solve2 :: Input -> Int
solve2 = maxTrail

tests2 :: [(String, Int)]
tests2 = [(testInput, 154)]

main :: IO ()
main = do
    s <- readFile "input/23.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
