module Main where

import Geometry
import Graph
import Utilities
import Data.Maybe
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = Map Position Int

parse :: String -> Input
parse s = Map.fromList [(p, digitToInt c) | (p, c) <- readGrid s]

-- Part One

-- Directions

data Direction = N | E | S | W
    deriving (Bounded, Enum, Eq, Ord, Show)
 
-- one position in the direction
oneStep :: Direction -> Position
oneStep N = Position 0 (-1)
oneStep E = Position 1 0
oneStep S = Position 0 1
oneStep W = Position (-1) 0

-- turn left or right
turns :: Direction -> [Direction]
turns N = [E, W]
turns S = [E, W]
turns E = [N, S]
turns W = [N, S]

-- state of the crucible
data State = State {
    position :: Position,
    direction :: !Direction,
    num_steps :: !Int -- number of steps already taken in this direction
    }
    deriving (Eq, Ord, Show)

-- start by moving from the top left corner
startStates :: [State]
startStates = [State zero E 0, State zero S 0]

-- possible moves, without considering bounds
moves :: Int -> Int -> State -> [State]
moves max_straight min_turn (State p d n) =
    [State (p .+. oneStep d) d (n+1) | n < max_straight] ++
    [State (p .+. oneStep d') d' 1 | n >= min_turn, d' <- turns d]

-- states reachable from here in one step, and their costs
nextStates :: Int -> Int -> Map Position Int ->
    State -> [(Int, State)]
nextStates max_straight min_turn blocks s =
    [(c, s') |
        s' <- moves max_straight min_turn s,
        c <- maybeToList (Map.lookup (position s') blocks)]

-- minimum cost from any start state to a finish state
minCost :: Int -> Int -> Map Position Int -> Int
minCost max_straight min_turn blocks =
    head [cost |
        (cost, s) <- shortestPaths (nextStates max_straight min_turn blocks) startStates,
        at_end s]
  where
    finish = fst (Map.findMax blocks)
    at_end (State p _ n) = p == finish && n >= min_turn

solve1 :: Input -> Int
solve1 = minCost 3 1

testInput1 :: String
testInput1 = "\
    \2413432311323\n\
    \3215453535623\n\
    \3255245654254\n\
    \3446585845452\n\
    \4546657867536\n\
    \1438598798454\n\
    \4457876987766\n\
    \3637877979653\n\
    \4654967986887\n\
    \4564679986453\n\
    \1224686865563\n\
    \2546548887735\n\
    \4322674655533\n"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 102)]

-- Part Two

solve2 :: Input -> Int
solve2 = minCost 10 4

testInput2 :: String
testInput2 = "\
    \111111111111\n\
    \999999999991\n\
    \999999999991\n\
    \999999999991\n\
    \999999999991\n"

tests2 :: [(String, Int)]
tests2 = [(testInput1, 94), (testInput2, 71)]

main :: IO ()
main = do
    s <- readFile "input/17.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
